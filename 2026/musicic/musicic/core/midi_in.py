"""Read a MIDI file into quantized NoteSets, one per track."""
from __future__ import annotations

import io

import mido

from .notes import NoteSet
from .quantize import QuantizeReport, quantize


def _tempo_and_meta(midi: mido.MidiFile):
    tempo, num, den, key = 500000, 4, 4, None
    for track in midi.tracks:
        for msg in track:
            if msg.is_meta:
                if msg.type == "set_tempo" and tempo == 500000:
                    tempo = msg.tempo
                elif msg.type == "time_signature":
                    num, den = msg.numerator, msg.denominator
                elif msg.type == "key_signature":
                    key = msg.key
    return tempo, num, den, key


_KEYS = {"C": 0, "C#": 1, "Db": 1, "D": 2, "D#": 3, "Eb": 3, "E": 4, "F": 5,
         "F#": 6, "Gb": 6, "G": 7, "G#": 8, "Ab": 8, "A": 9, "A#": 10,
         "Bb": 10, "B": 11}


def _parse_key(key: str | None) -> tuple[int | None, bool]:
    if not key:
        return None, False
    minor = key.endswith("m")
    root = key[:-1] if minor else key
    return _KEYS.get(root), minor


def read_midi(data: bytes | str, **qopts) -> tuple[list[NoteSet], list[QuantizeReport]]:
    """Parse MIDI bytes (or a path) into one NoteSet per non-empty track."""
    midi = mido.MidiFile(file=io.BytesIO(data)) if isinstance(data, bytes) \
        else mido.MidiFile(data)
    tempo, num, den, key = _tempo_and_meta(midi)
    root, minor = _parse_key(key)
    bpm = round(60_000_000 / tempo, 3)

    sets: list[NoteSet] = []
    reports: list[QuantizeReport] = []

    for i, track in enumerate(midi.tracks):
        # Pair note-ons with note-offs, tracking absolute tick position.
        ongoing: dict[int, list[int]] = {}
        raw: list[tuple[int, int, int]] = []
        clock = 0
        name = ""
        for msg in track:
            clock += msg.time
            if msg.is_meta and msg.type == "track_name":
                name = msg.name.strip()
            elif msg.type == "note_on" and msg.velocity > 0:
                ongoing.setdefault(msg.note, []).append(clock)
            elif msg.type == "note_off" or (msg.type == "note_on" and msg.velocity == 0):
                starts = ongoing.get(msg.note)
                if starts:
                    start = starts.pop(0)
                    if clock > start:
                        raw.append((start, clock - start, msg.note))
        if not raw:
            continue

        notes, report = quantize(raw, midi.ticks_per_beat, **qopts)
        sets.append(NoteSet(
            notes=notes,
            name=name or f"Track {i + 1}",
            grid=report.grid,
            tempo_bpm=bpm, numerator=num, denominator=den,
            key_root=root, key_is_minor=minor,
            source=getattr(midi, "filename", "") or "uploaded",
        ))
        reports.append(report)

    return sets, reports
