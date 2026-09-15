"""Key signatures, pitch spelling and accidental display.

Notation shows an accidental only when a note differs from what the key
signature and the rest of the bar already established. Without that rule every
chromatic note is marked every time it appears, which is what turns a page of
music into a hedge of sharps.
"""
from __future__ import annotations

LETTERS = "CDEFGAB"
LETTER_PC = {"C": 0, "D": 2, "E": 4, "F": 5, "G": 7, "A": 9, "B": 11}

# Major key by tonic pitch class -> (name, accidentals in signature).
# Positive counts are sharps, negative are flats.
_MAJOR = {0: ("C", 0), 7: ("G", 1), 2: ("D", 2), 9: ("A", 3), 4: ("E", 4),
          11: ("B", 5), 6: ("F#", 6), 1: ("Db", -5), 8: ("Ab", -4),
          3: ("Eb", -3), 10: ("Bb", -2), 5: ("F", -1)}
_MINOR = {9: ("Am", 0), 4: ("Em", 1), 11: ("Bm", 2), 6: ("F#m", 3),
          1: ("C#m", 4), 8: ("G#m", 5), 3: ("D#m", 6), 10: ("Bbm", -5),
          5: ("Fm", -4), 0: ("Cm", -3), 7: ("Gm", -2), 2: ("Dm", -1)}

_SHARP_ORDER = ["F", "C", "G", "D", "A", "E", "B"]
_FLAT_ORDER = ["B", "E", "A", "D", "G", "C", "F"]


class Key:
    """A key signature, and how to spell pitches inside it."""

    def __init__(self, root: int | None = 0, minor: bool = False):
        table = _MINOR if minor else _MAJOR
        self.name, self.count = table.get((root or 0) % 12, ("C", 0))
        self.minor = minor
        # Which letters the signature alters, and by how much.
        self.alter: dict[str, int] = {ltr: 0 for ltr in LETTERS}
        if self.count > 0:
            for ltr in _SHARP_ORDER[:self.count]:
                self.alter[ltr] = 1
        else:
            for ltr in _FLAT_ORDER[:-self.count]:
                self.alter[ltr] = -1
        self.uses_flats = self.count < 0

    @property
    def display(self) -> str:
        return self.name if self.minor else f"{self.name} major"

    def spell(self, pitch: int) -> tuple[str, int, int]:
        """MIDI pitch -> (letter, alteration, octave).

        A pitch the signature already covers keeps its own letter. Anything else
        is written as an alteration of a neighbouring letter, leaning the way the
        key does: flats in flat keys, sharps in sharp keys.
        """
        pc, octave = pitch % 12, pitch // 12 - 1
        # 1. A letter whose key-signature form is exactly this pitch: no mark.
        for ltr in LETTERS:
            if (LETTER_PC[ltr] + self.alter[ltr]) % 12 == pc:
                return ltr, self.alter[ltr], _octave_for(ltr, pitch)
        # 2. A letter's own natural pitch: written as a natural. This is the
        #    case a sharp key needs for, say, D in F# major.
        for ltr in LETTERS:
            if LETTER_PC[ltr] % 12 == pc:
                return ltr, 0, _octave_for(ltr, pitch)
        # 3. Otherwise alter a neighbour, leaning the way the key does.
        order = (-1, 1) if self.uses_flats else (1, -1)
        for delta in order:
            for ltr in LETTERS:
                if (LETTER_PC[ltr] + delta) % 12 == pc:
                    return ltr, delta, _octave_for(ltr, pitch)
        return "C", 0, octave


def _octave_for(letter: str, pitch: int) -> int:
    """Octave number for a spelling, handling B# and Cb crossing the boundary."""
    octave = pitch // 12 - 1
    pc = pitch % 12
    base = LETTER_PC[letter]
    if base == 0 and pc == 11:        # C flat, sounds as B below
        return octave + 1
    if base == 11 and pc == 0:        # B sharp, sounds as C above
        return octave - 1
    return octave


_GLYPH = {-2: "bb", -1: "b", 0: "n", 1: "#", 2: "##"}


class BarAccidentals:
    """Tracks what has already been marked in the current bar."""

    def __init__(self, key: Key):
        self.key = key
        self.state: dict[tuple[str, int], int] = {}

    def reset(self) -> None:
        self.state.clear()

    def needed(self, letter: str, alter: int, octave: int) -> str | None:
        """The accidental to draw, or None when the bar already says it."""
        current = self.state.get((letter, octave), self.key.alter[letter])
        if alter == current:
            return None
        self.state[(letter, octave)] = alter
        return _GLYPH[alter]


def guess_key(notes, minor: bool = False) -> int:
    """Pick the tonic whose diatonic set holds the most weight."""
    weight = [0] * 12
    for n in notes:
        if n.pitch:
            weight[n.pitch % 12] += n.dur
    scale = (0, 2, 3, 5, 7, 8, 10) if minor else (0, 2, 4, 5, 7, 9, 11)
    best, best_score = 0, (-1, -1)
    for root in range(12):
        fit = sum(weight[(root + s) % 12] for s in scale)
        score = (fit, weight[root])
        if score > best_score:
            best, best_score = root, score
    return best
