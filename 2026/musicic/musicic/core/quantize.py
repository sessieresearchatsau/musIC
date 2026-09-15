"""Snap raw MIDI ticks onto the musical grid.

This automates the step done by hand in the notebooks, e.g. Zelda's
    {56 -> 2^2, 113 -> 2^3, 227 -> 2^4, 455 -> 2^5, {y_,27,x_} :> {y,2,x}}

Raw durations fall *short* of their nominal value because sequencers lift the
note-off early for articulation: a 480-tick eighth is written as 455, a 960-tick
quarter as 911. Rounding to the nearest grid unit therefore gives 30, not 32, so
we snap to the nearest value in the musical lattice instead and let the report
show how far each snap had to travel.
"""
from __future__ import annotations

from dataclasses import dataclass

from .notes import GRID, duration_name


def lattice(grid: int = GRID, allow_dotted: bool = True,
            allow_triplet: bool = True) -> list[int]:
    """Durations a notator can actually write, in grid units.

    Only exact values are admitted: at grid 32 an eighth triplet would be
    32/3 units, so it is silently absent and such a piece needs grid 48 or 96.
    """
    out: set[int] = set()
    base = grid * 4  # whole note
    while base >= 1:
        out.add(base)
        if allow_dotted and base * 3 % 2 == 0:
            out.add(base * 3 // 2)
        if allow_triplet and base * 2 % 3 == 0:
            out.add(base * 2 // 3)
        base //= 2
    return sorted(out)


@dataclass
class Snap:
    """One raw value and where it landed."""
    raw: int
    units: int
    count: int          # how many notes used this raw value
    error: float        # relative distance travelled, as a fraction
    name: str

    @property
    def suspicious(self) -> bool:
        return self.error > 0.12


@dataclass
class QuantizeReport:
    """The derived mapping, for display and override."""
    scale: float                  # grid units per raw tick
    ticks_per_beat: int
    grid: int
    duration_map: list[Snap]
    onset_error: float            # worst onset displacement, in grid units

    @property
    def worst_error(self) -> float:
        return max((s.error for s in self.duration_map), default=0.0)

    def as_rules(self) -> str:
        """Render as the Mathematica replacement rules the notebooks write by hand."""
        return "{" + ", ".join(f"{s.raw} -> {s.units}" for s in self.duration_map) + "}"


def snap_duration(value: float, allowed: list[int]) -> tuple[int, float]:
    """Nearest lattice value, plus the relative distance travelled."""
    if value <= 0:
        return allowed[0], 1.0
    best = min(allowed, key=lambda a: abs(a - value))
    return best, abs(best - value) / max(value, 1e-9)


def quantize(raw_notes: list[tuple[int, int, int]], ticks_per_beat: int,
             grid: int = GRID, onset_grid: int | None = None,
             allow_dotted: bool = True, allow_triplet: bool = True,
             overrides: dict[int, int] | None = None):
    """Map (start_ticks, dur_ticks, pitch) onto the grid.

    `onset_grid` is the finest onset subdivision to snap to, in grid units;
    it defaults to a 32nd note. Returns (notes, report).
    """
    from .notes import Note

    scale = grid / ticks_per_beat
    if onset_grid is None:
        onset_grid = max(1, grid // 8)
    allowed = lattice(grid, allow_dotted=allow_dotted, allow_triplet=allow_triplet)
    overrides = overrides or {}

    # Build the duration map once per distinct raw value, so the report is a
    # short table rather than one row per note.
    counts: dict[int, int] = {}
    for _, dur, _ in raw_notes:
        counts[dur] = counts.get(dur, 0) + 1

    mapping: dict[int, Snap] = {}
    for raw in sorted(counts):
        if raw in overrides:
            units, err = overrides[raw], 0.0
        else:
            units, err = snap_duration(raw * scale, allowed)
        mapping[raw] = Snap(raw, units, counts[raw], err, duration_name(units, grid))

    notes: list[Note] = []
    worst_onset = 0.0
    for start, dur, pitch in raw_notes:
        exact = start * scale
        onset = int(round(exact / onset_grid)) * onset_grid
        worst_onset = max(worst_onset, abs(onset - exact))
        notes.append(Note(onset, mapping[dur].units, pitch))

    notes.sort(key=lambda n: (n.start, n.pitch))
    report = QuantizeReport(
        scale=scale,
        ticks_per_beat=ticks_per_beat,
        grid=grid,
        duration_map=sorted(mapping.values(), key=lambda s: -s.count),
        onset_error=worst_onset,
    )
    return notes, report
