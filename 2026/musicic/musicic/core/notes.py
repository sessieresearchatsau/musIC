"""Note and NoteSet: the canonical in-memory representation.

Time is measured in *grid units*. GRID is how many units make one quarter note;
the default of 32 matches the research notebooks, where a 480-ppq quarter (455
raw ticks after the articulation gap) is written as 32, a dotted eighth as 24,
and a 16th as 8.

A grid of 32 cannot express triplets exactly -- an eighth triplet would need
32/3 units. Use 48, 96 or 192 when a piece has them.
"""
from __future__ import annotations

from dataclasses import dataclass, field, replace
from fractions import Fraction

GRID = 32  # default units per quarter note (notebook convention)
REST = 0   # pitch value marking a rest (0 is not a usable MIDI pitch)

# Note value as a multiple of a quarter note.
_BASE_NAMES = [
    (Fraction(4), "whole"), (Fraction(2), "half"), (Fraction(1), "quarter"),
    (Fraction(1, 2), "eighth"), (Fraction(1, 4), "16th"), (Fraction(1, 8), "32nd"),
    (Fraction(1, 16), "64th"), (Fraction(1, 32), "128th"),
]


def duration_name(units: int, grid: int = GRID) -> str:
    """Human name for a duration in grid units ('dotted eighth', 'eighth triplet')."""
    q = Fraction(units, grid)          # length in quarter notes
    for base, name in _BASE_NAMES:
        if q == base:
            return name
        if q == base * 3 / 2:
            return f"dotted {name}"
        if q == base * 7 / 4:
            return f"double-dotted {name}"
        if q == base * 2 / 3:
            return f"{name} triplet"
    return f"{units}u"


_PC_NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]


def pitch_name(pitch: int) -> str:
    """MIDI number -> scientific pitch name. 0 is our rest marker."""
    if pitch == REST:
        return "rest"
    return f"{_PC_NAMES[pitch % 12]}{pitch // 12 - 1}"


@dataclass(frozen=True, slots=True)
class Note:
    start: int   # onset, grid units from the beginning
    dur: int     # sounding length, grid units
    pitch: int   # MIDI note number, or REST

    @property
    def end(self) -> int:
        return self.start + self.dur

    def beat(self, grid: int = GRID) -> Fraction:
        return Fraction(self.start, grid)

    def describe(self, grid: int = GRID) -> str:
        return (f"{pitch_name(self.pitch)} · {duration_name(self.dur, grid)} "
                f"· beat {float(self.beat(grid)) + 1:g}")


@dataclass
class NoteSet:
    """One musical voice: a time-ordered list of notes plus its provenance."""
    notes: list[Note] = field(default_factory=list)
    name: str = "untitled"
    grid: int = GRID                 # units per quarter note
    tempo_bpm: float = 120.0
    numerator: int = 4
    denominator: int = 4
    key_root: int | None = None      # pitch class of the tonic, if known
    key_is_minor: bool = False
    source: str = ""                 # file or expression this came from

    def __len__(self) -> int:
        return len(self.notes)

    def sorted(self) -> "NoteSet":
        return replace(self, notes=sorted(self.notes, key=lambda n: (n.start, n.pitch)))

    @property
    def span(self) -> int:
        return max((n.end for n in self.notes), default=0)

    def chords(self) -> list[list[Note]]:
        """Group notes into simultaneities, ordered by onset."""
        groups: dict[int, list[Note]] = {}
        for n in self.notes:
            groups.setdefault(n.start, []).append(n)
        return [sorted(groups[k], key=lambda n: n.pitch) for k in sorted(groups)]

    def is_monophonic(self) -> bool:
        return all(len(c) == 1 for c in self.chords())

    def top_line(self) -> "NoteSet":
        """Reduce chords to their highest note -- the usual melodic reading."""
        return replace(self, notes=[c[-1] for c in self.chords()],
                       name=f"{self.name} (top line)")

    def voice(self, index: int) -> "NoteSet":
        """Split by chord position: 0 = lowest voice, -1 = highest."""
        out = []
        for c in self.chords():
            if -len(c) <= index < len(c):
                out.append(c[index])
        return replace(self, notes=out, name=f"{self.name} (voice {index})")

    def clipped(self) -> tuple["NoteSet", int]:
        """Truncate overlapping notes so each ends where the next begins.

        The {duration, pitch} form has no way to say "these two overlap": its
        reader advances the clock by each duration in turn. A line with legato
        overlaps must therefore be clipped before it can be written that way, so
        we do it explicitly and return how many notes were shortened.
        """
        notes = sorted(self.notes, key=lambda n: (n.start, n.pitch))
        out, clipped = [], 0
        for i, n in enumerate(notes):
            limit = notes[i + 1].start if i + 1 < len(notes) else n.end
            if n.end > limit > n.start:
                out.append(Note(n.start, limit - n.start, n.pitch))
                clipped += 1
            else:
                out.append(n)
        return replace(self, notes=out), clipped

    def monophonic_line(self) -> tuple["NoteSet", dict]:
        """One strictly sequential voice with explicit rests -- the input the
        pair-style encodings need. Reports what had to be discarded."""
        chords = self.chords()
        dropped = sum(len(c) - 1 for c in chords)
        line, clipped = self.top_line().clipped()
        return line.with_rests(), {"chord_notes_dropped": dropped,
                                   "notes_clipped": clipped}

    def with_rests(self) -> "NoteSet":
        """Insert explicit rests into gaps, so start times can be dropped
        without losing the rhythm."""
        out: list[Note] = []
        cursor = 0
        for chord in self.chords():
            start = chord[0].start
            if start > cursor:
                out.append(Note(cursor, start - cursor, REST))
            out.extend(chord)
            cursor = max(cursor, max(n.end for n in chord))
        return replace(self, notes=out)

    def sounding(self) -> "NoteSet":
        """Only the notes that make a sound -- rests removed."""
        return replace(self, notes=[n for n in self.notes if n.pitch != REST])
