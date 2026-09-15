"""Reduce a list of rows to nested indexed concatenations.

A port of ReduceSetList / improveReduction / FindSeqFns from SSSiCv102.wl,
keeping its three escalating passes:

  1. runs of one identical element            -> the repLen = 1 pass
  2. exactly repeated blocks of length p >= 2 -> the exact repLen >= 2 pass
  3. blocks that agree once the varying       -> the generic pass, which blanks
     integers are blanked, with a formula        every integer except 1 before
     fitted to each varying column               comparing

Correctness rests on the same trick the Wolfram code uses: nothing is accepted
until expanding it reproduces the input exactly. FindSeqFns ends with

    If[ExpandAll[ans] === ExpandAll[subseqList], ans, subseqList]

and every transformation here is guarded the same way, so the output either
expands back to the original rows or is discarded. `reduce_rows` asserts this
over the whole result before returning.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from fractions import Fraction

import sympy

from .findseq import fit, is_extrapolation
from .ic import IC, Iter, expand, render

Row = tuple
Item = Row | IC

MAX_ROWS = 4000        # guard: the passes are quadratic in the row count


# --------------------------------------------------------------------------
# helpers
# --------------------------------------------------------------------------

def _key(item: Item) -> str:
    """Structural identity, used to compare items for exact repetition."""
    return render(item)


def _expands_to(items: list[Item], want: list[Row]) -> bool:
    try:
        got = expand(list(items))
    except Exception:
        return False
    return _norm(got) == _norm(want)


def _norm(rows) -> list:
    out = []
    for r in rows:
        if isinstance(r, (list, tuple)):
            out.append(tuple(_flat(v) for v in r))
        else:
            out.append(_flat(r))
    return out


def _flat(v):
    if isinstance(v, Fraction) and v.denominator == 1:
        return int(v)
    if isinstance(v, sympy.Expr) and v.is_number:
        r = sympy.nsimplify(v)
        return int(r) if r.is_Integer else Fraction(int(r.p), int(r.q))
    return v


def _length(item: Item) -> int:
    """How many rows this item expands to."""
    return len(expand([item]))


# --------------------------------------------------------------------------
# pass 1 -- runs of one identical item
# --------------------------------------------------------------------------

def _pass_runs(items: list[Item]) -> list[Item]:
    out: list[Item] = []
    i = 0
    while i < len(items):
        j = i + 1
        k = _key(items[i])
        while j < len(items) and _key(items[j]) == k:
            j += 1
        n = j - i
        out.append(IC([items[i]], Iter(None, 1, n)) if n > 1 else items[i])
        i = j
    return out


# --------------------------------------------------------------------------
# pass 2 -- exactly repeated blocks
# --------------------------------------------------------------------------

def _pass_blocks(items: list[Item]) -> list[Item]:
    keys = [_key(x) for x in items]
    n = len(items)
    for p in range(2, n // 2 + 1):
        i = 0
        while i + 2 * p <= n:
            if keys[i:i + p] == keys[i + p:i + 2 * p]:
                reps = 2
                while i + (reps + 1) * p <= n and \
                        keys[i:i + p] == keys[i + reps * p:i + (reps + 1) * p]:
                    reps += 1
                block = items[i:i + p]
                return (items[:i]
                        + [IC(block, Iter(None, 1, reps))]
                        + items[i + reps * p:])
            i += 1
    return items


# --------------------------------------------------------------------------
# pass 3 -- the generic pass
# --------------------------------------------------------------------------

def _skeleton(item: Item) -> str:
    """ReduceSetList's genericization: every integer except 1 becomes 0, so
    rows that differ only in their varying numbers compare equal."""
    if isinstance(item, IC):
        return "IC(" + ",".join(_skeleton(a) for a in item.args) + ")"
    return "{" + ",".join("1" if v == 1 else "0" for v in item) + "}"


def _numbers(item: Item) -> list:
    if isinstance(item, IC):
        return [v for a in item.args for v in _numbers(a)]
    return list(item)


def _rebuild(item: Item, values: list, pos: list[int]) -> Item:
    """Rebuild `item` with its numeric leaves replaced, in order."""
    it = iter(values)

    def walk(x):
        if isinstance(x, IC):
            return IC([walk(a) for a in x.args], x.it)
        return tuple(next(it) for _ in x)
    return walk(item)


def _pass_generic(items: list[Item], var_no: int,
                  extrapolations: list[str] | None = None,
                  allow_extrapolation: bool = False) -> tuple[list[Item], int]:
    n = len(items)
    skels = [_skeleton(x) for x in items]

    # Collect every candidate and keep the one covering the most rows. Taking
    # the first match instead is what stops a small local repeat from hiding a
    # larger grouping -- the paper's eq. 2 needs the p=4 reading, not the p=1
    # one that happens to appear earlier in the list.
    best = None
    for p in range(1, n // 2 + 1):
        i = 0
        while i + 2 * p <= n:
            if skels[i:i + p] == skels[i + p:i + 2 * p]:
                reps = 2
                while i + (reps + 1) * p <= n and \
                        skels[i:i + p] == skels[i + reps * p:i + (reps + 1) * p]:
                    reps += 1
                cover = reps * p
                # More coverage wins; on a tie the *shorter* repeating unit
                # wins, since more repetitions of a smaller block compress
                # further (8 copies of one row beat 2 copies of four).
                if best is None or (cover, -p) > (best[0], -best[1]):
                    best = (cover, p, i, reps)
                i += reps * p
            else:
                i += 1
    if best is None:
        return items, var_no

    _, p, i, reps = best
    span = items[i:i + reps * p]
    node = _fit_block(span, p, reps, var_no, extrapolations, allow_extrapolation)
    if node is None:
        return items, var_no
    return items[:i] + [node] + items[i + reps * p:], var_no + 1


def _fit_block(span: list[Item], p: int, reps: int, var_no: int,
               extrapolations: list[str] | None = None,
               allow_extrapolation: bool = False) -> IC | None:
    """FindSeqFns: transpose the blocks into columns, fit each, verify."""
    blocks = [span[k * p:(k + 1) * p] for k in range(reps)]
    var = f"n{var_no}"
    n = sympy.Symbol(var)

    template: list[Item] = []
    for slot in range(p):
        first = blocks[0][slot]
        cols = [_numbers(b[slot]) for b in blocks]
        if any(len(c) != len(cols[0]) for c in cols):
            return None
        values: list = []
        for c in range(len(cols[0])):
            series = [col[c] for col in cols]
            if len(set(series)) == 1:
                values.append(series[0])
                continue
            if not all(isinstance(v, (int, Fraction)) for v in series):
                return None
            # min_extra=0 matches Mathematica, which the notebooks rely on;
            # a fit with no spare point to test is recorded as a guess.
            guess = is_extrapolation(series)
            if guess and not allow_extrapolation:
                # Only two differing values: any straight line fits and none is
                # evidence. Refusing here is what keeps unstructured music from
                # "reducing" into a formula that means nothing.
                return None
            f = fit(series, var, 1, min_extra=0 if allow_extrapolation else 1)
            if f is None:
                return None
            if guess and extrapolations is not None:
                extrapolations.append(f"{var}: {series} -> {f}")
            values.append(f)
        template.append(_rebuild(first, values, []))

    node = IC(template, Iter(var, 1, reps))
    return node if _expands_to([node], _norm(expand(list(span)))) else None


# --------------------------------------------------------------------------
# improvement -- merge neighbours, absorb on the right
# --------------------------------------------------------------------------

def _merge_adjacent(items: list[Item]) -> list[Item]:
    """Two counted ICs over the same body become one, as improveReductionAt's
    SequenceReplace does."""
    out: list[Item] = []
    for item in items:
        if (out and isinstance(item, IC) and isinstance(out[-1], IC)
                and item.it.is_count and out[-1].it.is_count
                and [_key(a) for a in item.args] == [_key(a) for a in out[-1].args]):
            prev = out[-1]
            out[-1] = IC(prev.args, Iter(None, 1, prev.it.stop + item.it.stop))
        else:
            out.append(item)
    return out


def _absorb_right(items: list[Item], original: list[Row]) -> list[Item]:
    """Extend an IC's index range over the items that follow it, when doing so
    still expands to the same thing."""
    changed = True
    while changed:
        changed = False
        for i, item in enumerate(items):
            if not isinstance(item, IC) or item.it.is_count:
                continue
            stop = item.it.stop
            if not isinstance(stop, int):
                continue
            while i + 1 < len(items):
                bigger = IC(item.args, Iter(item.it.var, item.it.start, stop + 1))
                grew = _length(bigger) - _length(item)
                if grew <= 0 or i + 1 + grew > len(items) + 1:
                    break
                trial = items[:i] + [bigger] + items[i + 1 + grew:]
                if len(items[i + 1:i + 1 + grew]) < grew or not _expands_to(trial, original):
                    break
                items, item, stop, changed = trial, bigger, stop + 1, True
            if changed:
                break
    return items


# --------------------------------------------------------------------------
# driver
# --------------------------------------------------------------------------

@dataclass
class Reduction:
    tree: list
    original: list
    passes: list[str]
    verified: bool
    extrapolations: list[str] = field(default_factory=list)

    @property
    def text(self) -> str:
        return render(self.tree)

    @property
    def size(self) -> int:
        return _count_leaves(self.tree)

    @property
    def ratio(self) -> float:
        return round(self.size / max(len(self.original), 1), 3)


def _count_leaves(tree) -> int:
    """Rows you would have to write down to state the reduced form."""
    total = 0
    for item in tree:
        total += 1 if not isinstance(item, IC) else _count_leaves(item.args)
    return total


def reduce_rows(rows: list[Row], max_rounds: int = 40,
                allow_extrapolation: bool = False) -> Reduction:
    """Reduce rows to nested ICs, never returning something that fails to
    expand back to the input."""
    original = _norm(rows)
    if len(rows) > MAX_ROWS:
        return Reduction(list(rows), original, ["skipped: too many rows"], True)

    items: list[Item] = [tuple(r) for r in rows]
    log: list[str] = []
    extrapolations: list[str] = []
    var_no = 1

    for _ in range(max_rounds):
        before = [_key(x) for x in items]

        for name, step in (("runs", _pass_runs),
                           ("blocks", _pass_blocks),
                           ("merge", _merge_adjacent)):
            trial = step(list(items))
            if [_key(x) for x in trial] != [_key(x) for x in items]:
                if _expands_to(trial, original):
                    items = trial
                    log.append(f"{name}: {len(items)} items")
                else:
                    log.append(f"{name}: rejected, did not expand back")

        trial, nxt = _pass_generic(list(items), var_no, extrapolations,
                                   allow_extrapolation)
        if [_key(x) for x in trial] != [_key(x) for x in items]:
            if _expands_to(trial, original):
                items, var_no = trial, nxt
                log.append(f"generic: {len(items)} items")
            else:
                log.append("generic: rejected, did not expand back")

        trial = _absorb_right(list(items), original)
        if [_key(x) for x in trial] != [_key(x) for x in items]:
            items = trial
            log.append(f"absorb: {len(items)} items")

        if [_key(x) for x in items] == before:
            break

    # Now reduce inside each counted IC body. An indexed body carries a free
    # index variable, so it has no standalone expansion to verify against and
    # is left alone.
    items = [_reduce_inside(x, log, extrapolations, allow_extrapolation)
             for x in items]
    if not _expands_to(items, original):
        items = [tuple(r) for r in rows]
        log.append("inner reduction rejected, kept outer form")

    ok = _expands_to(items, original)
    if not ok:                      # belt and braces; should be unreachable
        return Reduction([tuple(r) for r in rows], original,
                         log + ["FAILED verification, returned input"], False)
    return Reduction(items, original, log, True, extrapolations)


def _reduce_inside(item: Item, log: list[str], extrapolations: list[str],
                   allow_extrapolation: bool = False) -> Item:
    """Recurse into a counted IC's body, as ReduceSetList recurses on success."""
    if not isinstance(item, IC) or not item.it.is_count:
        return item
    body = expand(list(item.args))
    if len(body) < 2:
        return item
    inner = reduce_rows([tuple(r) if isinstance(r, (list, tuple)) else (r,)
                         for r in body], allow_extrapolation=allow_extrapolation)
    if inner.verified and inner.size < _count_leaves(item.args):
        log.append(f"inner: {_count_leaves(item.args)} -> {inner.size}")
        extrapolations.extend(inner.extrapolations)
        return IC(list(inner.tree), item.it)
    return item
