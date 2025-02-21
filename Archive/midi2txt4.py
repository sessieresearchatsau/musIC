#!/usr/bin/env python3

import sys
import math
from mido import MidiFile, tick2second

def parse_midi_file(midi_path):
    """
    Reads a MIDI file and returns a list of tracks, where each track is
    a list of note events. Each note event is a dict:
      {
        'start_sec': float,
        'end_sec': float,
        'note': int,       # raw MIDI note number, 0-127
        'velocity': int
      }
    """
    midi = MidiFile(midi_path)
    ticks_per_beat = midi.ticks_per_beat

    track_events = [[] for _ in range(len(midi.tracks))]
    absolute_ticks = [0] * len(midi.tracks)
    current_tempo = 500000  # default 120 BPM if none is set

    for i, track in enumerate(midi.tracks):
        note_on_cache = {}
        for msg in track:
            # Accumulate the delta time in ticks for this track
            absolute_ticks[i] += msg.time

            if msg.is_meta and msg.type == 'set_tempo':
                current_tempo = msg.tempo
            else:
                current_time_sec = tick2second(
                    absolute_ticks[i],
                    ticks_per_beat,
                    current_tempo
                )

                # Note On (velocity > 0)
                if msg.type == 'note_on' and msg.velocity > 0:
                    note_on_cache[msg.note] = (absolute_ticks[i], msg.velocity)

                # Note Off (velocity=0) or note_off type
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
                            'note': msg.note,      # raw MIDI note
                            'velocity': velocity
                        })
                        del note_on_cache[msg.note]

    return track_events

def generate_minimal_output(track_data_list, output_path):
    """
    Writes a text file in the format:

    TRACK X:
    {{duration,note,velocity},{duration,note,velocity}}

    Where 'note' is the raw MIDI note number. 
    """
    with open(output_path, 'w', encoding='utf-8') as f:
        for i, note_events in enumerate(track_data_list):
            # Sort the notes by start time
            note_events.sort(key=lambda e: e['start_sec'])

            f.write(f"TRACK {i+1}:\n")

            # Build the list of notes for this track
            sets_list = []
            for ev in note_events:
                duration = ev['end_sec'] - ev['start_sec']
                sets_list.append(
                    f"{{{duration:.3f},{ev['note']},{ev['velocity']}}}"
                )

            # If no notes, just print {{}} to indicate empty
            if sets_list:
                line = "{{" + ",".join(sets_list) + "}}"
            else:
                line = "{{}}"

            f.write(line + "\n\n")  # extra newline after each track

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