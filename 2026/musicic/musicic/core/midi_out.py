"""Write a NoteSet back out as a MIDI file."""
from __future__ import annotations

import io

import mido

from .notes import GRID, NoteSet

PPQ = 480          # what the corpus uses, so exports match what we read

# MIDI meta text is latin-1, and mido raises rather than substituting. Piece
# names here come from transcriptions and routinely carry an em dash or an
# accent, so fold anything unwritable down to ASCII instead of failing the
# export over the title.
_SUBS = {"\u2014": "-", "\u2013": "-", "\u2018": "'", "\u2019": "'",
         "\u201c": '"', "\u201d": '"', "\u2026": "...", "\u00b7": "-"}


def _meta_text(text: str) -> str:
    for bad, good in _SUBS.items():
        text = text.replace(bad, good)
    return text.encode("latin-1", "replace").decode("latin-1")


def to_midi(ns: NoteSet, instrument: int = 0) -> bytes:
    """Render a NoteSet as a single-track MIDI file.

    Grid units are scaled to PPQ ticks, so a piece written here and read back
    quantizes to exactly the set it started from.
    """
    grid = ns.grid or GRID
    scale = PPQ / grid

    mid = mido.MidiFile(ticks_per_beat=PPQ)
    track = mido.MidiTrack()
    mid.tracks.append(track)

    track.append(mido.MetaMessage("track_name",
                                  name=_meta_text(ns.name or "musIC")[:64],
                                  time=0))
    track.append(mido.MetaMessage("set_tempo",
                                  tempo=int(round(60_000_000 / (ns.tempo_bpm or 120))),
                                  time=0))
    track.append(mido.MetaMessage("time_signature", numerator=ns.numerator,
                                  denominator=ns.denominator, time=0))
    track.append(mido.Message("program_change", program=instrument, time=0))

    # Absolute events first, then deltas. Note-offs sort before note-ons at the
    # same tick so a repeated pitch retriggers instead of being cut short.
    events: list[tuple[int, int, int, int]] = []      # tick, order, pitch, on
    for n in ns.notes:
        if not n.pitch:
            continue                                   # a rest is just silence
        events.append((int(round(n.start * scale)), 1, n.pitch, 1))
        events.append((int(round(n.end * scale)), 0, n.pitch, 0))
    events.sort()

    clock = 0
    for tick, _, pitch, on in events:
        track.append(mido.Message("note_on" if on else "note_off",
                                  note=pitch, velocity=80 if on else 0,
                                  time=tick - clock))
        clock = tick

    end = int(round(ns.span * scale))
    track.append(mido.MetaMessage("end_of_track", time=max(0, end - clock)))

    buf = io.BytesIO()
    mid.save(file=buf)
    return buf.getvalue()
