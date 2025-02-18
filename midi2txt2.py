#!/usr/bin/env python3

import sys
import math
from mido import MidiFile, tick2second

# Simple note name lookup for MIDI note numbers:
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
    Reads a MIDI file and returns a list of tracks, where each track is
    a list of note events. Each note event is a dict:
      {
        'start_sec': float,
        'end_sec': float,
        'note': int,
        'velocity': int
      }
    """
    midi = MidiFile(midi_path)
    ticks_per_beat = midi.ticks_per_beat

    # We'll store note events per track in track_events[track_index].
    track_events = [[] for _ in range(len(midi.tracks))]

    # Track the "absolute ticks" for each track. (Type 1 MIDI keeps separate deltas.)
    absolute_ticks = [0] * len(midi.tracks)

    # Default tempo if not found is 500000 microseconds per beat (120 BPM)
    current_tempo = 500000

    for i, track in enumerate(midi.tracks):
        # note_on_cache to store {note_number: (start_tick, velocity)}
        note_on_cache = {}

        for msg in track:
            # Increase the absolute tick count for this track
            absolute_ticks[i] += msg.time

            if msg.is_meta:
                # Update tempo if there's a set_tempo message
                if msg.type == 'set_tempo':
                    current_tempo = msg.tempo
            else:
                # Convert current absolute ticks to seconds
                current_time_sec = tick2second(absolute_ticks[i],
                                               ticks_per_beat,
                                               current_tempo)

                # Check for note_on or note_off
                if msg.type == 'note_on' and msg.velocity > 0:
                    # Note ON
                    note_on_cache[msg.note] = (absolute_ticks[i], msg.velocity)

                elif (msg.type == 'note_on' and msg.velocity == 0) or msg.type == 'note_off':
                    # Note OFF
                    if msg.note in note_on_cache:
                        start_tick, velocity = note_on_cache[msg.note]
                        start_sec = tick2second(start_tick,
                                                ticks_per_beat,
                                                current_tempo)
                        end_sec = current_time_sec
                        # Save the note event
                        track_events[i].append({
                            'start_sec': start_sec,
                            'end_sec': end_sec,
                            'note': msg.note,
                            'velocity': velocity
                        })
                        # Remove from cache
                        del note_on_cache[msg.note]

    return track_events

def generate_minimal_output(track_data_list, output_path):
    """
    Writes a minimal text file with lines like:
    
      TRACK 1:
      (duration, note, velocity)
      (duration, note, velocity)
      ...
    
    for each track, sorted by start time.
    """
    with open(output_path, 'w', encoding='utf-8') as f:
        for i, note_events in enumerate(track_data_list):
            # Sort by start time
            note_events.sort(key=lambda e: e['start_sec'])

            # Print track header
            f.write(f"TRACK {i+1}:\n")

            for ev in note_events:
                duration = ev['end_sec'] - ev['start_sec']
                note_str = note_name(ev['note'])
                vel = ev['velocity']
                # Write the set: (duration, note, velocity)
                # Duration formatted to 3 decimals
                f.write(f"({duration:.3f}, {note_str}, {vel})\n")

            # Extra newline between tracks
            if i < len(track_data_list) - 1:
                f.write("\n")

def main():
    if len(sys.argv) < 3:
        print("Usage: python midi_to_text.py <input.mid> <output.txt>")
        sys.exit(1)
    
    midi_path = sys.argv[1]
    output_path = sys.argv[2]

    # Parse the MIDI
    track_data_list = parse_midi_file(midi_path)
    
    # Generate minimal output
    generate_minimal_output(track_data_list, output_path)

    print(f"Done! Created {output_path}")

if __name__ == "__main__":
    main()