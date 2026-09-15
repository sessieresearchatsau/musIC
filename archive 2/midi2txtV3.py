#!/usr/bin/env python3

import sys
from mido import MidiFile, tick2second
from fractions import Fraction

def float_to_fraction(value, max_denominator=1000):
    """
    Converts a floating-point number to a simplified fraction string.
    """
    return str(Fraction(value).limit_denominator(max_denominator))

def parse_midi_file(midi_path):
    """
    Parses the MIDI file and returns:
      - a list of tracks (each track is a list of note events)
      - ticks_per_beat from the MIDI file

    Each note event is a dict:
      {
        'start_sec': float,   # time in seconds when note starts
        'duration_ticks': int,# how many ticks the note lasted (accumulated)
        'note': int           # raw MIDI note number
      }
    """
    midi = MidiFile(midi_path)
    ticks_per_beat = midi.ticks_per_beat

    # Each element in track_events will hold a list of finished note events for that track.
    track_events = [[] for _ in range(len(midi.tracks))]

    # Keep track of current tempo (microseconds per beat); default is 120 BPM = 500000 us/beat
    current_tempo = 500000

    # For each track, we store:
    #   - ongoing_notes: dictionary of {note_number: { 'duration_ticks': int, 'start_sec': float }}
    #   - running_tick: the total tick count in this track so far
    #   - running_sec:  the total time in seconds so far
    ongoing_notes = [{} for _ in range(len(midi.tracks))]
    running_tick = [0] * len(midi.tracks)
    running_sec = [0.0] * len(midi.tracks)

    for i, track in enumerate(midi.tracks):

        for msg in track:
            # The delta time tells us how many ticks have passed since the previous event.
            delta_ticks = msg.time

            # Convert those delta ticks to seconds using the current tempo.
            # We'll add that time to our running_sec for this track.
            delta_seconds = tick2second(delta_ticks, ticks_per_beat, current_tempo)

            # 1) Accumulate the delta ticks as "duration" for all currently ON notes in this track
            for note_data in ongoing_notes[i].values():
                note_data['duration_ticks'] += delta_ticks

            # 2) Update the absolute tick and time counters
            running_tick[i] += delta_ticks
            running_sec[i] += delta_seconds

            # 3) Check if the message is a tempo change
            if msg.is_meta and msg.type == 'set_tempo':
                current_tempo = msg.tempo

            # 4) Check if it’s a note-on with velocity > 0 => start a new note
            elif msg.type == 'note_on' and msg.velocity > 0:
                # Record that this note is now ON, with zero duration so far.
                ongoing_notes[i][msg.note] = {
                    'duration_ticks': 0,
                    'start_sec': running_sec[i]  # store the current track time in seconds
                }

            # 5) Check if it’s a note-off or a note_on with velocity=0 => end that note
            elif (msg.type == 'note_off') or (msg.type == 'note_on' and msg.velocity == 0):
                if msg.note in ongoing_notes[i]:
                    # We finalize the note’s event
                    note_data = ongoing_notes[i][msg.note]
                    track_events[i].append({
                        'start_sec': note_data['start_sec'],
                        'duration_ticks': note_data['duration_ticks'],
                        'note': msg.note
                    })
                    # Remove it from the ongoing notes
                    del ongoing_notes[i][msg.note]

    return track_events, ticks_per_beat

def generate_minimal_output(track_data_list, ticks_per_beat, output_path):
    """
    Writes a text file with lines:

    TRACK X:
    {{start_in_seconds,fraction_of_whole_note,note},{...},...}

    Where the fraction_of_whole_note is computed by:
      duration_beats = duration_ticks / ticks_per_beat
      fraction_of_whole_note = duration_beats / 4
    """
    with open(output_path, 'w', encoding='utf-8') as f:
        for i, note_events in enumerate(track_data_list):
            f.write(f"TRACK {i+1}:\n")

            # Sort by start_sec (just for a cleaner chronological output).
            note_events.sort(key=lambda ev: ev['start_sec'])

            sets_list = []
            for ev in note_events:
                start_fraction = float_to_fraction(ev['start_sec'])

                # Convert total ticks to beats
                duration_beats = ev['duration_ticks'] / ticks_per_beat

                # Convert beats to fraction of a 4-beat whole note
                fraction_of_whole = duration_beats / 4

                duration_fraction = float_to_fraction(fraction_of_whole)
                note_num = ev['note']

                # Format as {start,duration,note}
                sets_list.append(f"{{{start_fraction},{duration_fraction},{note_num}}}")

            if sets_list:
                line = "{{" + ",".join(sets_list) + "}}"
            else:
                line = "{{}}"

            f.write(line + "\n\n")

def main():
    if len(sys.argv) < 3:
        print("Usage: python midi_to_text.py <input.mid> <output.txt>")
        sys.exit(1)

    midi_path = sys.argv[1]
    output_path = sys.argv[2]

    track_data_list, ticks_per_beat = parse_midi_file(midi_path)
    generate_minimal_output(track_data_list, ticks_per_beat, output_path)
    print(f"Done! Output written to: {output_path}")

if __name__ == "__main__":
    main()