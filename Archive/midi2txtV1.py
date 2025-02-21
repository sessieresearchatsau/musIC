#!/usr/bin/env python3

import sys
from mido import MidiFile, tick2second

def parse_midi_file(midi_path):
    """
    Reads a MIDI file and returns a list of tracks, where each track is
    a list of note events, each a dict:
      {
        'start_sec': float,
        'end_sec': float,
        'note': int  # raw MIDI note number
      }
    """
    midi = MidiFile(midi_path)
    ticks_per_beat = midi.ticks_per_beat

    track_events = [[] for _ in range(len(midi.tracks))]
    absolute_ticks = [0] * len(midi.tracks)
    current_tempo = 500000  # default tempo if none is set

    # A cache for note-on events: {note: start_tick}
    note_on_cache = [{} for _ in range(len(midi.tracks))]

    for i, track in enumerate(midi.tracks):
        for msg in track:
            # Accumulate delta time in ticks for this track
            absolute_ticks[i] += msg.time

            if msg.is_meta and msg.type == 'set_tempo':
                current_tempo = msg.tempo
            else:
                current_time_sec = tick2second(
                    absolute_ticks[i],
                    ticks_per_beat,
                    current_tempo
                )

                # Note On
                if msg.type == 'note_on' and msg.velocity > 0:
                    note_on_cache[i][msg.note] = absolute_ticks[i]

                # Note Off (or note_on with velocity=0)
                elif (msg.type == 'note_on' and msg.velocity == 0) or msg.type == 'note_off':
                    if msg.note in note_on_cache[i]:
                        start_tick = note_on_cache[i][msg.note]
                        start_sec = tick2second(
                            start_tick,
                            ticks_per_beat,
                            current_tempo
                        )
                        end_sec = current_time_sec

                        track_events[i].append({
                            'start_sec': start_sec,
                            'end_sec': end_sec,
                            'note': msg.note
                        })
                        del note_on_cache[i][msg.note]

    return track_events

def generate_minimal_output(track_data_list, output_path):
    """
    Writes a text file with lines like:

    TRACK 1:
    {{start,duration,note},{start,duration,note}}

    where start and duration are floats (3 decimal places), 
    and note is a raw MIDI note number.
    """
    with open(output_path, 'w', encoding='utf-8') as f:
        for i, note_events in enumerate(track_data_list):
            # Sort events by start time
            note_events.sort(key=lambda e: e['start_sec'])

            f.write(f"TRACK {i+1}:\n")

            sets_list = []
            for ev in note_events:
                start_sec = ev['start_sec']
                duration_sec = ev['end_sec'] - ev['start_sec']
                note_num = ev['note']

                # {start,duration,note}, no spaces
                sets_list.append(
                    f"{{{start_sec:.3f},{duration_sec:.3f},{note_num}}}"
                )

            if sets_list:
                line = "{{" + ",".join(sets_list) + "}}"
            else:
                line = "{{}}"

            f.write(line + "\n\n")  # one line per track, blank line after

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