"""Score an encoding by how compressible it looks.

These are cheap proxies for "will the IC reducer find anything here", chosen to
mirror what ReduceSetList actually exploits:

  runs            adjacent duplicates      -> the repLen=1 pass
  best_period     repeated blocks          -> the exact repLen>=2 pass
  generic_runs    repeats ignoring which   -> the generic pass, which blanks
                  integers vary               non-1 integers before matching
  distinct        alphabet size            -> how much there is to describe

`ratio` is the headline: reduced size over original size, low is good.
"""
from __future__ import annotations

from dataclasses import asdict, dataclass


@dataclass
class Metrics:
    n: int
    distinct: int
    runs: int
    best_period: int
    best_period_cover: float
    longest_repeat: int
    generic_runs: int
    ratio: float

    def as_dict(self) -> dict:
        return asdict(self)


def _runs(seq: list) -> int:
    if not seq:
        return 0
    return 1 + sum(1 for a, b in zip(seq, seq[1:]) if a != b)


def _best_period(seq: list) -> tuple[int, float]:
    """Smallest block length whose repeats cover the most of the sequence."""
    n = len(seq)
    best, cover = 0, 0.0
    for p in range(1, n // 2 + 1):
        covered = sum(p for i in range(0, n - p, p) if seq[i:i + p] == seq[i + p:i + 2 * p])
        if covered / max(n, 1) > cover + 1e-9:
            best, cover = p, covered / n
    return best, cover


def _longest_repeat(seq: list) -> int:
    """Longest block occurring more than once. Binary search on length."""
    n = len(seq)
    lo, hi, best = 1, n // 2, 0
    while lo <= hi:
        mid = (lo + hi) // 2
        seen = set()
        hit = False
        for i in range(n - mid + 1):
            k = tuple(seq[i:i + mid])
            if k in seen:
                hit = True
                break
            seen.add(k)
        if hit:
            best, lo = mid, mid + 1
        else:
            hi = mid - 1
    return best


def _genericize(seq: list) -> list:
    """ReduceSetList's generic pass: replace every integer except 1 with 0, so
    rows that differ only in their varying numbers compare equal."""
    return [tuple(0 if isinstance(v, int) and v != 1 else v for v in row)
            for row in seq]


def measure(rows: list) -> Metrics:
    seq = [tuple(r) for r in rows]
    n = len(seq)
    if n == 0:
        return Metrics(0, 0, 0, 0, 0.0, 0, 0, 1.0)
    period, cover = _best_period(seq)
    runs = _runs(seq)
    generic = _runs(_genericize(seq))
    longest = _longest_repeat(seq)

    # Headline: how few rows would remain if we collapsed the structure these
    # proxies can see. Runs handle adjacent duplicates; a covering period
    # collapses its repeats to one block.
    reduced = runs
    if period and cover > 0.5:
        reduced = min(reduced, int(n * (1 - cover)) + period)
    return Metrics(n, len(set(seq)), runs, period, round(cover, 3),
                   longest, generic, round(reduced / n, 3))
