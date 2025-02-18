#!/usr/bin/env python3

import sys
import math
from mido import MidiFile, tick2second

# Simple note name lookup for MIDI note numbers:
NOTE_NAMES = ["C","C#","D","D#","E","F",
              "F#","G","G#","A","A#","B"]

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

    track_events = [[] for _ in range(len(midi.tracks))]
    absolute_ticks = [0] * len(midi.tracks)
    current_tempo = 500000  # default 120 BPM

    for i, track in enumerate(midi.tracks):
        note_on_cache = {}

        for msg in track:
            # Update the absolute tick count
            absolute_ticks[i] += msg.time

            if msg.is_meta and msg.type == 'set_tempo':
                current_tempo = msg.tempo
            else:
                current_time_sec = tick2second(absolute_ticks[i],
                                               ticks_per_beat,
                                               current_tempo)
                # Note On
                if msg.type == 'note_on' and msg.velocity > 0:
                    note_on_cache[msg.note] = (absolute_ticks[i], msg.velocity)
                # Note Off
                elif (msg.type == 'note_on' and msg.velocity == 0) or msg.type == 'note_off':
                    if msg.note in note_on_cache:
                        start_tick, velocity = note_on_cache[msg.note]
                        start_sec = tick2second(start_tick,
                                                ticks_per_beat,
                                                current_tempo)
                        end_sec = current_time_sec
                        track_events[i].append({
                            'start_sec': start_sec,
                            'end_sec': end_sec,
                            'note': msg.note,
                            'velocity': velocity
                        })
                        del note_on_cache[msg.note]

    return track_events

def generate_minimal_output(track_data_list, output_path):
    """
    Writes a text file in the format:
    
    TRACK 1:
    {{duration,note,velocity},{duration,note,velocity}}
    
    TRACK 2:
    {{...}}
    
    etc., with all notes for each track on a single line, 
    sorted by start time, no spaces, double curly braces.
    """
    with open(output_path, 'w', encoding='utf-8') as f:
        for i, note_events in enumerate(track_data_list):
            # Sort by start time
            note_events.sort(key=lambda e: e['start_sec'])

            f.write(f"TRACK {i+1}:\n")

            # Build a list of note strings
            note_strs = []
            for ev in note_events:
                duration = ev['end_sec'] - ev['start_sec']
                note_label = note_name(ev['note'])
                vel = ev['velocity']
                # No spaces, duration to 3 decimals
                note_strs.append(f"{{{duration:.3f},{note_label},{vel}}}")

            # If there are no notes, we just show {{}} as empty
            if note_strs:
                line = "{{" + ",".join(note_strs) + "}}"
            else:
                line = "{{}}"

            f.write(f"{line}\n\n")  # blank line after each track

def main():
    if len(sys.argv) < 3:
        print("Usage: python midi_to_text.py <input.mid> <output.txt>")
        sys.exit(1)
    
    midi_path = sys.argv[1]
    output_path = sys.argv[2]

    track_data_list = parse_midi_file(midi_path)
    generate_minimal_output(track_data_list, output_path)
    print(f"Done! Output written to: {output_path}")

if __name__ == "__main__":
    main()