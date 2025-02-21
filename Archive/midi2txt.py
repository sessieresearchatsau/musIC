#!/usr/bin/env python3

import sys
import math
from mido import MidiFile, tempo2bpm, tick2second

# For note name conversion
NOTE_NAMES = ["C", "C#", "D", "D#", "E", "F", 
              "F#", "G", "G#", "A", "A#", "B"]


def note_name(note_number):
    """
    Convert a MIDI note number (0-127) into a string like "C4".
    Middle C (MIDI note 60) is C4.
    """
    if note_number < 0 or note_number > 127:
        return f"OutOfRange({note_number})"
    name = NOTE_NAMES[note_number % 12]
    octave = (note_number // 12) - 1
    return f"{name}{octave}"


def parse_midi_file(midi_path):
    """
    Parses the MIDI file and returns a list (per track) of note events.
    Each track’s result is a list of dicts with:
        {
          'start_sec': float,
          'end_sec': float,
          'note': int,
          'velocity': int,
        }
    Also returns ticks_per_beat (int).
    """
    midi = MidiFile(midi_path)
    ticks_per_beat = midi.ticks_per_beat
    
    # We’ll store one list per track: track_events[track_index] = [list of note events]
    track_events = [[] for _ in range(len(midi.tracks))]
    
    # We'll accumulate absolute time (in ticks) per track separately.
    # If the MIDI is Type 1, each track has its own event.delta_time, 
    # so we need separate counters for each track.
    absolute_ticks = [0] * len(midi.tracks)
    
    # We'll also track the current tempo (microseconds per quarter note)
    # Default MIDI tempo is 500000 (120 bpm) if none is specified.
    current_tempo = 500000
    
    # We could parse tempo changes from track 0, but to handle them properly
    # we really need to read each track event in the correct order. So let’s 
    # do a "simultaneous" read by merging events in time. But for simplicity, 
    # we'll just do each track sequentially, acknowledging that real-time 
    # interplay can be more complex. This is a simpler approach.
    
    for i, track in enumerate(midi.tracks):
        # Dictionary to remember when a note was turned on:
        # note_on_cache[note_number] = (start_tick, velocity)
        note_on_cache = {}
        
        for msg in track:
            absolute_ticks[i] += msg.time  # accumulate delta time in ticks
            
            if msg.is_meta:
                # If we get a set_tempo message, update current_tempo
                if msg.type == 'set_tempo':
                    current_tempo = msg.tempo  # microseconds per quarter note
            else:
                # Convert ticks -> seconds
                current_time_sec = tick2second(absolute_ticks[i], 
                                               ticks_per_beat, 
                                               current_tempo)
                
                # If it's a MIDI note-on or note-off
                if msg.type == 'note_on' and msg.velocity > 0:
                    # A real note-on
                    note_on_cache[msg.note] = (absolute_ticks[i], msg.velocity)
                    
                elif (msg.type == 'note_on' and msg.velocity == 0) or msg.type == 'note_off':
                    # This is effectively a note-off
                    if msg.note in note_on_cache:
                        start_tick, velocity = note_on_cache[msg.note]
                        start_sec = tick2second(start_tick, ticks_per_beat, current_tempo)
                        end_sec = current_time_sec
                        
                        # Store the note event
                        track_events[i].append({
                            'start_sec': start_sec,
                            'end_sec': end_sec,
                            'note': msg.note,
                            'velocity': velocity
                        })
                        # Remove from cache
                        del note_on_cache[msg.note]
    
    return track_events, ticks_per_beat


def generate_report(track_data_list, output_path, time_step=0.1):
    """
    Creates a text report with:
    1) A table of note events for each track (time in seconds, note, velocity).
    2) An ASCII piano roll for each track (using a specified time_step).
    
    track_data_list: list of lists -> track_data_list[i] = list of note dicts
    output_path: output .txt file path
    time_step: resolution for ASCII roll in seconds
    """
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("MIDI to Text Report\n")
        f.write("=========================================\n\n")
        
        # For each track
        for track_index, note_events in enumerate(track_data_list):
            f.write(f"TRACK {track_index + 1}:\n")
            f.write("-----------------------------------------\n")
            
            if not note_events:
                f.write("  (No note events)\n\n")
                continue
            
            # Sort note events by start_sec
            note_events.sort(key=lambda e: e['start_sec'])
            
            # Write a table of note events
            f.write("Detailed Note Events:\n")
            f.write("--------------------------------------------------\n")
            f.write(f"{'Start(s)':>10}  {'End(s)':>10}  {'Note':>6}  {'Vel':>4}\n")
            f.write("--------------------------------------------------\n")
            
            min_note = 127
            max_note = 0
            max_time = 0.0
            
            for ev in note_events:
                start_sec = ev['start_sec']
                end_sec   = ev['end_sec']
                note_num  = ev['note']
                vel       = ev['velocity']
                
                # Track min/max note/time
                if note_num < min_note: 
                    min_note = note_num
                if note_num > max_note: 
                    max_note = note_num
                if end_sec > max_time:
                    max_time = end_sec
                
                # Write table row
                f.write(f"{start_sec:10.3f}  {end_sec:10.3f}  "
                        f"{note_name(note_num):>6}  {vel:4}\n")
            
            # Add ASCII piano roll
            f.write("\nASCII Piano Roll:\n")
            f.write("(time step = {:.2f}s)\n\n".format(time_step))
            
            # If no valid notes, skip roll
            if min_note > max_note:
                f.write("  (No valid notes)\n\n")
                continue
            
            # Discretize time
            total_steps = int(math.ceil(max_time / time_step)) + 1
            
            # Create 2D grid: row = each note, col = time steps
            note_range = max_note - min_note + 1
            piano_grid = [[" " for _ in range(total_steps)] for _ in range(note_range)]
            
            # Fill the grid with '#' for active times
            for ev in note_events:
                start_col = int(ev['start_sec'] // time_step)
                end_col   = max(start_col, int(math.ceil(ev['end_sec'] / time_step)) - 1)
                row       = max_note - ev['note']  # invert so highest note is at top
                
                for c in range(start_col, min(end_col + 1, total_steps)):
                    piano_grid[row][c] = "#"
            
            # Optional: Print a time ruler
            f.write("Time (s) : ")
            for c in range(total_steps):
                current_sec = c * time_step
                # Mark each 1 second boundary with '|'
                if abs((current_sec % 1.0)) < 1e-6:
                    f.write("|")
                else:
                    f.write(" ")
            f.write("\n")
            
            # Print the actual piano roll lines
            for r in range(note_range):
                midi_note_num = max_note - r
                line_label = f"{note_name(midi_note_num):>5} : "
                f.write(line_label)
                row_chars = "".join(piano_grid[r])
                f.write(row_chars + "\n")
            
            f.write("\n")  # Separate tracks with extra newline
        f.write("End of Report\n")

def main():
    """
    Command-line usage:
        python midi_to_text.py input.mid output.txt
    """
    if len(sys.argv) < 3:
        print("Usage: python midi_to_text.py <input.mid> <output.txt>")
        sys.exit(1)
    
    midi_path = sys.argv[1]
    output_path = sys.argv[2]
    
    print(f"Reading MIDI file: {midi_path}")
    track_data_list, tpb = parse_midi_file(midi_path)
    print("Generating text report...")
    generate_report(track_data_list, output_path, time_step=0.1)
    print(f"Done! Output written to: {output_path}")


if __name__ == "__main__":
    main()
