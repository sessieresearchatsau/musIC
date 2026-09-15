"""Encoders: derive alternative set representations from one NoteSet.

The premise, carried over from the ICN paper's edge difference set list, is that
a pattern invisible in absolute values often becomes obvious under differences.
The paper puts it this way: grouping edges by originating vertex and taking the
differences of vertex numbers "suddenly reveals new vistas for the pattern
seeker". These encoders are the musical version of that move, and the bench in
`metrics.py` says which one actually paid off for a given piece.

Every encoder returns rows of integers (or a list of such lists). Encoders that
can be inverted set `invertible = True` and provide `decode`, so a set edited in
the UI can be rendered back as notation.
"""
from __future__ import annotations

from dataclasses import dataclass
from fractions import Fraction
from typing import Callable

from .notes import GRID, REST, Note, NoteSet

# Sentinel for the interval encoding's pitch column. A real melodic interval is
# bounded by the MIDI range, so 1000 cannot collide with one. Needed because
# dpitch = 0 would otherwise be ambiguous between a rest, the first note, and a
# repeated unison.
REST_DP = 1000

Row = tuple[int, ...]
Encoded = list[Row] | list[list[Row]]


@dataclass
class Encoding:
    """The output of an encoder, plus what is needed to read it back."""
    rows: Encoded
    columns: tuple[str, ...]
    encoder: str
    nested: bool = False              # rows is a list of groups, EDSL-style
    anchor: dict | None = None        # data needed to invert (first pitch, etc.)
    labels: list[str] | None = None   # per-group labels when nested
    # The note each row describes, parallel to `flat`. This is what lets a
    # hover on row i light up the right notehead: without it the score would
    # group events its own way and the indices would drift apart wherever a
    # note was clipped or a chord collapsed.
    source_notes: list | None = None

    @property
    def flat(self) -> list[Row]:
        if not self.nested:
            return self.rows  # type: ignore[return-value]
        return [r for group in self.rows for r in group]  # type: ignore


@dataclass
class Encoder:
    key: str
    title: str
    blurb: str
    columns: tuple[str, ...]
    fn: Callable[[NoteSet], Encoding]
    invertible: bool = False
    nested: bool = False
    caveat: str = ""          # what inversion does not restore
    needs_polyphony: bool = False


REGISTRY: dict[str, Encoder] = {}


def register(key, title, blurb, columns, invertible=False, nested=False,
             caveat="", needs_polyphony=False):
    def deco(fn):
        REGISTRY[key] = Encoder(key, title, blurb, columns, fn, invertible,
                                nested, caveat, needs_polyphony)
        return fn
    return deco


def _seq(ns: NoteSet) -> list[Note]:
    """Monophonic reading with explicit rests, the input most encoders want.

    Order matters: reduce to one line *first*, then find the gaps. Doing it the
    other way lets a sustained inner voice fill a gap the melody really has, and
    the rest goes missing. Overlaps are clipped, since this form cannot express
    them -- `NoteSet.monophonic_line` reports how much that cost.
    """
    return ns.monophonic_line()[0].notes


# --------------------------------------------------------------------------
# Absolute encodings -- what the notebooks already use.
# --------------------------------------------------------------------------

@register("triple", "Absolute triple", "{start, duration, pitch} -- the full "
          "lossless form, and the only one that survives polyphony.",
          ("start", "duration", "pitch"), invertible=True)
def enc_triple(ns: NoteSet) -> Encoding:
    notes = ns.sorted().notes
    rows = [(n.start, n.dur, n.pitch) for n in notes]
    return Encoding(rows, ("start", "duration", "pitch"), "triple",
                    source_notes=notes)


@register("pair", "Absolute pair", "{duration, pitch} with rests written as "
          "pitch 0. Drops start times; the form the notebooks reduce.",
          ("duration", "pitch"), invertible=True)
def enc_pair(ns: NoteSet) -> Encoding:
    notes = _seq(ns)
    rows = [(n.dur, n.pitch) for n in notes]
    return Encoding(rows, ("duration", "pitch"), "pair", source_notes=notes)


@register("pair_flip", "Pitch-first pair", "{pitch, duration} -- the ordering "
          "the notebooks switch to just before reducing.",
          ("pitch", "duration"), invertible=True)
def enc_pair_flip(ns: NoteSet) -> Encoding:
    notes = _seq(ns)
    rows = [(n.pitch, n.dur) for n in notes]
    return Encoding(rows, ("pitch", "duration"), "pair_flip", source_notes=notes)


# --------------------------------------------------------------------------
# Difference encodings -- the direct analogue of the EDSL.
# --------------------------------------------------------------------------

@register("interval", "Melodic interval", "{duration, Δpitch}. Absolute pitch "
          "is replaced by the step from the previous note, so a transposed "
          "repeat of a phrase becomes an exact repeat. Rests carry Δ=1000.",
          ("duration", "dpitch"), invertible=True)
def enc_interval(ns: NoteSet) -> Encoding:
    notes = _seq(ns)
    rows, prev = [], None
    for n in notes:
        if n.pitch == REST:
            rows.append((n.dur, REST_DP))
        else:
            rows.append((n.dur, 0 if prev is None else n.pitch - prev))
            prev = n.pitch
    first = next((n.pitch for n in notes if n.pitch != REST), 60)
    return Encoding(rows, ("duration", "dpitch"), "interval",
                    anchor={"first_pitch": first}, source_notes=notes)


@register("ioi", "Inter-onset interval", "{Δstart, pitch}. Time between "
          "successive attacks rather than sounding length, so rests need no "
          "encoding at all.", ("ioi", "pitch"), invertible=True,
          caveat="restores rhythm and pitch, but not articulation: every note "
                 "comes back legato, filling the gap to the next attack")
def enc_ioi(ns: NoteSet) -> Encoding:
    chords = ns.top_line().sorted().notes
    rows = []
    for i, n in enumerate(chords):
        nxt = chords[i + 1].start if i + 1 < len(chords) else n.end
        rows.append((nxt - n.start, n.pitch))
    return Encoding(rows, ("ioi", "pitch"), "ioi",
                    anchor={"start": chords[0].start if chords else 0},
                    source_notes=chords)


@register("ratio", "Duration ratio", "{duration ratio ×1000, Δpitch}. A "
          "geometric rhythm collapses to a constant column -- the notebook's "
          "€ⁱ{62, 24/3ⁱ} is exactly this shape.",
          ("ratio_x1000", "dpitch"))
def enc_ratio(ns: NoteSet) -> Encoding:
    notes = _seq(ns)
    rows, prev_d, prev_p = [], None, None
    for n in notes:
        r = 1000 if prev_d in (None, 0) else int(round(1000 * n.dur / prev_d))
        dp = 0 if prev_p is None or n.pitch == REST else n.pitch - prev_p
        rows.append((r, dp))
        prev_d = n.dur
        if n.pitch != REST:
            prev_p = n.pitch
    return Encoding(rows, ("ratio_x1000", "dpitch"), "ratio", source_notes=notes)


@register("contour", "Contour", "{duration, sign of Δpitch}: -1 down, 0 same, "
          "+1 up. Coarse and lossy, but maximally repetitive.",
          ("duration", "contour"))
def enc_contour(ns: NoteSet) -> Encoding:
    notes = _seq(ns)
    rows, prev = [], None
    for n in notes:
        if n.pitch == REST:
            rows.append((n.dur, 0))
            continue
        d = 0 if prev is None else (n.pitch > prev) - (n.pitch < prev)
        rows.append((n.dur, d))
        prev = n.pitch
    return Encoding(rows, ("duration", "contour"), "contour", source_notes=notes)


# --------------------------------------------------------------------------
# Key-relative encodings.
# --------------------------------------------------------------------------

_MAJOR = {0: 1, 2: 2, 4: 3, 5: 4, 7: 5, 9: 6, 11: 7}
_MINOR = {0: 1, 2: 2, 3: 3, 5: 4, 7: 5, 8: 6, 10: 7}


def _degree(pitch: int, root: int, minor: bool) -> int | None:
    """Diatonic degree, counting octaves, or None if chromatic."""
    table = _MINOR if minor else _MAJOR
    rel = pitch - root
    pc, octave = rel % 12, rel // 12
    if pc not in table:
        return None
    return table[pc] + 7 * octave


@register("degree", "Scale degree", "{duration, diatonic degree}. Folds the "
          "irregular chromatic step pattern into an even ladder, so scalar "
          "runs become arithmetic. Chromatic notes are flagged.",
          ("duration", "degree"))
def enc_degree(ns: NoteSet) -> Encoding:
    root = ns.key_root if ns.key_root is not None else _guess_root(ns)
    notes = _seq(ns)
    rows, chromatic = [], 0
    for n in notes:
        if n.pitch == REST:
            rows.append((n.dur, 0))
            continue
        d = _degree(n.pitch, root, ns.key_is_minor)
        if d is None:
            chromatic += 1
            d = 0
        rows.append((n.dur, d))
    return Encoding(rows, ("duration", "degree"), "degree",
                    anchor={"root": root, "minor": ns.key_is_minor,
                            "chromatic": chromatic}, source_notes=notes)


def _guess_root(ns: NoteSet) -> int:
    """Krumhansl-style key guess: score each root by how much weight lands on
    its diatonic set, tie-broken by weight on the tonic itself."""
    weight = [0] * 12
    for n in ns.notes:
        if n.pitch != REST:
            weight[n.pitch % 12] += n.dur
    best, best_score = 0, (-1, -1)
    table = _MINOR if ns.key_is_minor else _MAJOR
    for root in range(12):
        fit = sum(weight[(root + pc) % 12] for pc in table)
        score = (fit, weight[root])
        if score > best_score:
            best, best_score = root, score
    return best


@register("pc", "Pitch class", "{duration, pitch mod 12}. Octave-folded, so a "
          "figure repeated an octave away becomes an exact repeat.",
          ("duration", "pc"))
def enc_pc(ns: NoteSet) -> Encoding:
    notes = _seq(ns)
    rows = [(n.dur, n.pitch % 12 if n.pitch != REST else -1) for n in notes]
    return Encoding(rows, ("duration", "pc"), "pc", source_notes=notes)


# --------------------------------------------------------------------------
# The EDSL analogue: group, then difference within each group.
# --------------------------------------------------------------------------

@register("edsl_pitch", "EDSL by pitch",
          "Group notes by pitch the way the paper groups edges by originating "
          "vertex, then take onset differences inside each group. Yields one "
          "set per pitch -- the nested list-of-sets shape ReduceSetList "
          "consumes.", ("onset differences",), nested=True)
def enc_edsl_pitch(ns: NoteSet) -> Encoding:
    by_pitch: dict[int, list[int]] = {}
    for n in ns.sorted().notes:
        if n.pitch != REST:
            by_pitch.setdefault(n.pitch, []).append(n.start)
    groups, labels = [], []
    from .notes import pitch_name
    for pitch in sorted(by_pitch):
        starts = by_pitch[pitch]
        diffs = [(b - a,) for a, b in zip(starts, starts[1:])]
        groups.append(diffs)
        labels.append(f"{pitch_name(pitch)} ({pitch})")
    return Encoding(groups, ("onset differences",), "edsl_pitch",
                    nested=True, labels=labels)


@register("edsl_onset", "EDSL by onset",
          "Group by onset (one set per attack point), listing the pitch "
          "intervals sounding there. The polyphonic counterpart -- chords "
          "become multi-element sets, exactly like a vertex with several "
          "outgoing edges.", ("intervals above bass",), nested=True,
          needs_polyphony=True)
def enc_edsl_onset(ns: NoteSet) -> Encoding:
    groups, labels = [], []
    for chord in ns.chords():
        bass = chord[0].pitch
        groups.append([(n.pitch - bass,) for n in chord[1:]])
        labels.append(f"t={chord[0].start}")
    return Encoding(groups, ("intervals above bass",), "edsl_onset",
                    nested=True, labels=labels)


# --------------------------------------------------------------------------
# Inversion: set -> notes, so any invertible encoding renders as notation.
# --------------------------------------------------------------------------

def decode(enc: Encoding, grid: int = GRID) -> list[Note]:
    """Rebuild notes from an encoding. Raises for lossy encoders."""
    rows = [tuple(int(v) for v in r) for r in enc.flat]
    kind = enc.encoder
    out: list[Note] = []

    if kind == "triple":
        return [Note(s, d, p) for s, d, p in rows]

    if kind in ("pair", "pair_flip"):
        t = 0
        for row in rows:
            d, p = (row[0], row[1]) if kind == "pair" else (row[1], row[0])
            # Keep rests as real entries. Dropping them and rebuilding from the
            # gaps between notes loses any rest that is not between two notes --
            # a bar ending in a rest, or a piece starting with one.
            out.append(Note(t, d, p))
            t += d
        return out

    if kind == "interval":
        t = 0
        pitch = (enc.anchor or {}).get("first_pitch", 60)
        started = False
        for d, dp in rows:
            if dp == REST_DP:          # a rest keeps its place in the line
                out.append(Note(t, d, REST))
                t += d
                continue
            if started:
                pitch += dp
            started = True
            out.append(Note(t, d, pitch))
            t += d
        return out

    if kind == "ioi":
        t = (enc.anchor or {}).get("start", 0)
        for i, (gap, p) in enumerate(rows):
            if p != REST:
                out.append(Note(t, gap, p))
            t += gap
        return out

    raise ValueError(f"encoder {kind!r} is lossy and cannot be decoded")


def encode(ns: NoteSet, key: str) -> Encoding:
    if key not in REGISTRY:
        raise KeyError(f"unknown encoder {key!r}; have {sorted(REGISTRY)}")
    return REGISTRY[key].fn(ns)
