"""Lay a NoteSet out as measures a notation renderer can draw.

Produces plain dicts so the browser side stays dumb: it receives note heads,
duration codes and tie flags, and only has to draw them. Every emitted note
carries `row`, the index of the set row it came from, which is what links a
hover on the set to a hover on the staff.
"""
from __future__ import annotations

from fractions import Fraction

from .keysig import BarAccidentals, Key, guess_key
from .notes import GRID, REST, Note, NoteSet, pitch_name

# Duration in quarter notes -> renderer code.
_CODES = [
    (Fraction(4), "w"), (Fraction(2), "h"), (Fraction(1), "q"),
    (Fraction(1, 2), "8"), (Fraction(1, 4), "16"), (Fraction(1, 8), "32"),
    (Fraction(1, 16), "64"),
]

def vex_key(letter: str, alter: int, octave: int) -> str:
    """VexFlow key string, e.g. ('B', -1, 4) -> 'bb/4'."""
    mark = {-2: "bb", -1: "b", 0: "", 1: "#", 2: "##"}[alter]
    return f"{letter.lower()}{mark}/{octave}"


def decompose(quarters: Fraction) -> list[tuple[str, int]]:
    """Split a length into writable (code, dots) pieces, largest first.

    A duration with no single symbol -- five sixteenths, say -- comes back as
    several pieces, which the caller renders as tied notes.
    """
    out: list[tuple[str, int]] = []
    remaining = quarters
    guard = 0
    while remaining > 0 and guard < 16:
        guard += 1
        for base, code in _CODES:
            for dots, mult in ((2, Fraction(7, 4)), (1, Fraction(3, 2)), (0, Fraction(1))):
                length = base * mult
                if length <= remaining:
                    out.append((code, dots))
                    remaining -= length
                    break
            else:
                continue
            break
        else:
            break
    return out


def layout(ns: NoteSet, max_measures: int | None = None,
           source_notes: list[Note] | None = None, spare: int = 0,
           page: int = 0) -> dict:
    """Group notes into measures of renderable notes.

    When `source_notes` is given, the layout follows exactly that sequence and
    each drawn note carries the indices of the rows that produced it, so a
    hover links the two views reliably. Chords map one drawn note to several
    rows; a rest inserted to fill a gap maps to none.

    `spare` appends that many empty bars. An empty piece always gets at least
    one bar, because the editor needs a staff on screen before there is
    anything to click on.

    `max_measures` and `page` window the result. Drawing a 600-note piece in one
    go is what made the browser crawl, so long pieces are paged instead.
    """
    grid = ns.grid or GRID
    beats_per_measure = Fraction(ns.numerator * 4, ns.denominator)   # in quarters
    measure_units = int(beats_per_measure * grid)

    if source_notes is None:
        source_notes = ns.with_rests().sorted().notes

    # Group by onset: a chord is several pitches sharing one drawn note.
    groups: dict[int, list[tuple[int, Note]]] = {}
    for i, n in enumerate(source_notes):
        groups.setdefault(n.start, []).append((i, n))

    events: list[dict] = []
    for start in sorted(groups):
        members = groups[start]
        pitches = sorted({n.pitch for _, n in members if n.pitch != REST})
        events.append({
            "start": start,
            "dur": min(n.dur for _, n in members),
            "pitches": pitches,
            "rows": [i for i, _ in members],
        })

    key = Key(ns.key_root if ns.key_root is not None
              else guess_key(ns.notes, ns.key_is_minor), ns.key_is_minor)
    bar_acc = BarAccidentals(key)

    measures: list[dict] = []
    cursor = 0
    idx = 0
    while idx < len(events):
        m_start = cursor
        m_end = m_start + measure_units
        notes: list[dict] = []
        filled = 0
        bar_acc.reset()          # accidentals last only to the end of the bar

        while idx < len(events) and events[idx]["start"] < m_end:
            ev = events[idx]
            # Pad a hole before this event with a rest.
            if ev["start"] > m_start + filled:
                gap = ev["start"] - (m_start + filled)
                for code, dots in decompose(Fraction(gap, grid)):
                    notes.append(_mk(None, code, dots, [], False, key, bar_acc,
                                     m_start + filled, grid))
                filled += gap

            avail = m_end - ev["start"]
            take = min(ev["dur"], avail)
            pieces = decompose(Fraction(take, grid))
            at = ev["start"]
            for j, (code, dots) in enumerate(pieces):
                tie = (j < len(pieces) - 1) or (take < ev["dur"])
                notes.append(_mk(ev["pitches"], code, dots, ev["rows"], tie,
                                 key, bar_acc, at, grid))
            filled += take

            if take < ev["dur"]:          # spills past the barline
                ev["dur"] -= take
                ev["start"] = m_end
                break
            idx += 1

        # Pad the tail of the measure.
        if filled < measure_units and (notes or idx < len(events)):
            for code, dots in decompose(Fraction(measure_units - filled, grid)):
                notes.append(_mk(None, code, dots, [], False, key, bar_acc,
                                 m_start + filled, grid))

        if notes:
            measures.append({"index": len(measures), "notes": notes})
        cursor = m_end
        # Build every bar; max_measures now sizes a page rather than truncating,
        # so page 2 exists.
        if len(measures) > 4000:
            break

    # Always leave a staff to look at -- and to click into while composing.
    for _ in range(max(spare, 0) + (1 if not measures else 0)):
        empty = [_mk(None, code, dots, [], False, key, bar_acc, 0, grid)
                 for code, dots in decompose(Fraction(measure_units, grid))]
        measures.append({"index": len(measures), "notes": empty})

    total = len(measures)
    per_page = max_measures or total or 1
    pages = max(1, -(-total // per_page)) if total else 1
    page = max(0, min(page, pages - 1))
    shown = measures[page * per_page:(page + 1) * per_page]

    return {
        "measures": shown,
        "numerator": ns.numerator,
        "denominator": ns.denominator,
        "tempo": ns.tempo_bpm,
        "name": ns.name,
        "grid": grid,
        "key": key.name,
        "key_display": key.display,
        "page": page,
        "pages": pages,
        "per_page": per_page,
        "total_measures": total,
        "beat_units": int(Fraction(4, ns.denominator) * grid),
    }


def _mk(pitches, code, dots, rows, tie, key, bar_acc, at, grid) -> dict:
    rest = not pitches
    keys, accs = [], []
    for p in pitches or []:
        letter, alter, octave = key.spell(p)
        keys.append(vex_key(letter, alter, octave))
        accs.append(bar_acc.needed(letter, alter, octave))
    return {
        "keys": ["b/4"] if rest else keys,
        "accidentals": accs,          # None where the bar already said it
        "code": code,
        "dots": dots,
        "rest": rest,
        "rows": rows or [],
        "tie": tie,
        # Which beat this note sits in, so the renderer can beam by beat.
        "beat": at // max(1, int(Fraction(4, 4) * grid)) if grid else 0,
        "start": at,
        "label": "rest" if rest else ", ".join(pitch_name(p) for p in pitches),
    }
