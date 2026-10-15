"""Named approaches to concatenating a long set, and what to measure them by.

One set has many valid indexed concatenations. Which one a search returns is
decided by three choices, and each approach is labelled by them:

  how a block is chosen      G  greedy-shortest: the shortest repeating pattern
                                is fixed first, and nothing fixed is revisited
                                (ReduceSetList)
                             S  best-saving: every candidate is scored and the
                                one that saves the most is kept
                             B  bar-anchored: no piece crosses a bar line, and
                                the bars are what the levels above group
                             V  variant-merging: repeats that differ by a piece
                                still line up, the piece switched off where it
                                is absent

  how levels are built       F  flat, then nested: the biggest block over the
                                raw rows, then the search goes inside its body
                             U  up, level by level: short pieces first, then
                                groups of pieces, then groups of those

  what € may do              E1 repeat and index, with polynomial formulas
                             E2 a count that comes out 0 or 1 in some repeats,
                                so a piece is switched off or written once
                             E3 a € inside a number, as a term of a sum:
                                67 + €(j-2)[10] is 67, 67, 67, 77
                             E4 a € inside a row: {1, €(3)[3]} is {1, 3, 3, 3}

Every approach may produce E1; the code says what else it sets out to use,
and `measure` reports what a result actually used.
"""
from __future__ import annotations

import time
from dataclasses import dataclass, field

from . import concat, levels, reduce
from .ic import IC, Arith, Seq, _bindings, expand, render, render_wl, spans


@dataclass(frozen=True)
class Strategy:
    code: str
    name: str
    blurb: str
    choose: str                # G, S, B or V
    build: str                 # F or U
    tools: str                 # the € tools it sets out to use, beyond E1
    needs_bars: bool = False
    options: dict = field(default_factory=dict)


# A change between repeats is weighed against one more part: 0.4 of a part in
# the level search, 0.1 where gates are allowed. At 0.4 a gate is never worth
# its numbers, and the forms the gates exist for are never chosen.
GATED = {"gates": True, "specials": 2, "moving_cost": 0.1}

STRATEGIES = [
    Strategy("G·F", "Greedy-shortest", "ReduceSetList, ported: the shortest repeating "
             "pattern is fixed first and nothing fixed is revisited, so a small block can "
             "stop a bigger one.", "G", "F", ""),
    Strategy("S·F", "Fewest rows", "The block that saves the most rows is kept, the rows "
             "either side are searched again, and then the bodies of blocks. Compact, but "
             "with no levels.", "S", "F", "E4"),
    Strategy("S·U", "Level by level", "Short pieces, then groups of pieces, the cheapest "
             "grouping at each level: the way the Notebook is used.", "S", "U", "E2"),
    Strategy("B·U", "Bar by bar", "As S·U, but no piece crosses a bar line and each bar "
             "becomes one part, so the levels above group whole bars, then phrases.",
             "B", "U", "E2", needs_bars=True, options={"bars": True}),
    Strategy("S·U·E", "Gated levels", "As S·U, and a number that changes between repeats "
             "may step, alternate, or differ in one repeat, written with a € inside it.",
             "S", "U", "E2 E3", options=GATED),
    Strategy("V·U·E", "Variants", "As S·U·E, and repeats that differ by a piece -- missing "
             "in some, extra in some -- still line up, the piece switched off where it is "
             "absent.", "V", "U", "E2 E3", options={**GATED, "variants": True}),
    Strategy("B·V·U·E", "Bars and variants", "Bar by bar, with gates and variants.",
             "B", "U", "E2 E3", needs_bars=True,
             options={**GATED, "variants": True, "bars": True}),
]
BY_CODE = {s.code: s for s in STRATEGIES}


@dataclass
class Outcome:
    strategy: Strategy
    tree: list
    verified: bool
    seconds: float
    levels: list | None = None          # per level, its parts, for a U approach
    measures: dict = field(default_factory=dict)

    @property
    def text(self) -> str:
        return render(self.tree).replace("**", "^")


def run(code: str, rows, bars=None, on_event=None) -> Outcome:
    """Concatenate `rows` the way `code` names. `bars` (the bar of each row)
    is needed by the bar-anchored approaches and used by the measures."""
    st = BY_CODE[code]
    rows = [tuple(int(x) for x in r) for r in rows]
    if st.needs_bars and bars is None:
        raise ValueError(f"{code} needs to know where the bar lines are")
    t0 = time.perf_counter()
    lv = None
    if st.build == "F" and st.choose == "G":
        res = reduce.reduce_rows(rows)
        tree, ok = res.tree, res.verified
    elif st.build == "F":
        res = concat.concatenate(rows, on_event)
        tree, ok = res.tree, res.verified
    else:
        o = dict(st.options)
        if o.pop("bars", False):
            o["bars"] = tuple(bars)
        res = levels.find(rows, on_event, levels.Options(**o))
        tree, ok, lv = res.tree, res.verified, res.level_parts()
    secs = time.perf_counter() - t0
    ok = ok and [tuple(x) for x in expand(list(tree))] == rows
    out = Outcome(st, list(tree), ok, round(secs, 2), lv)
    out.measures = measure(out.tree, rows, bars)
    return out


# --------------------------------------------------------------------------
# measures
# --------------------------------------------------------------------------

def measure(tree: list, rows: list, bars=None) -> dict:
    """What a result costs and what it is made of.

    rows      rows written down (a row is 1, a € the rows of its body)
    numbers   numbers and index names written, iterators included
    parts     items at the top
    depth     €s nested inside one another, at most
    formulas  entries of rows that are formulas rather than plain numbers
    tools     the € tools used: E1-E4 (see the module docstring)
    bar_fit   of the blocks of two rows or more (every copy counted), the
              share that stays inside one bar or runs from a bar line to a
              bar line; None without bars
    """
    tools = _tools(tree)
    sp = spans(tree, limit=20000)
    return {"rows": concat.leaves(tree), "numbers": concat.written(tree),
            "parts": len(tree), "depth": 1 + max((s["depth"] for s in sp), default=-1),
            "formulas": _formulas(tree), "tools": sorted(tools),
            "bar_fit": _bar_fit(sp, bars, len(rows))}


def _bar_fit(sp: list, bars, n: int):
    if bars is None:
        return None
    bars = list(bars)

    def line(b: int) -> bool:          # is there a bar line before row b
        return b == 0 or b == n or bars[b] != bars[b - 1]

    blocks = [s for s in sp if s["rows"] >= 2]
    if not blocks:
        return None
    good = sum(1 for s in blocks
               if bars[s["start"]] == bars[s["end"]] or (line(s["start"]) and line(s["end"] + 1)))
    return round(good / len(blocks), 3)


def _formulas(tree) -> int:
    count = 0

    def walk(v):
        nonlocal count
        if isinstance(v, IC):
            for a in v.args:
                walk(a)
        elif isinstance(v, (list, tuple)):
            for a in v:
                walk(a)
        elif not isinstance(v, int):
            count += 1
    walk(tree)
    return count


def _tools(tree) -> set:
    """Which € tools a tree uses. E2 is found by expanding: a € whose count
    comes out 0 (or 1, for a count written as a formula) in some repeat."""
    used: set = set()

    def scalar(v, env):
        if isinstance(v, Arith):
            used.add("E3")
            for p in v.parts:
                node(p, env)

    def node(item, env):
        if isinstance(item, IC):
            used.add("E1")
            it = item.it
            for lim in (it.start, it.stop):
                scalar(lim, env)
            try:
                binds = _bindings(it, env)
            except Exception:                      # noqa: BLE001
                return
            varies = not isinstance(it.stop, int) or not isinstance(it.start, int)
            if varies and len(binds) <= 1:
                used.add("E2")
            for b in binds:
                for a in item.args:
                    node(a, {**env, **b})
        elif isinstance(item, (list, tuple)) and not isinstance(item, Seq):
            for x in item:
                if isinstance(x, IC) and _in_row(item):
                    used.add("E4")
                node(x, env) if isinstance(x, (IC, list, tuple)) else scalar(x, env)
        elif isinstance(item, Seq):
            for x in item:
                node(x, env)

    for item in tree:
        node(item, {})
    return used


def _in_row(item) -> bool:
    """A row is a list of numbers; a € among numbers splices into the row."""
    return any(not isinstance(x, (IC, list, tuple)) for x in item)


def bars_of(enc, ns) -> list | None:
    """The bar of each row of an encoding of `ns`, from the note the row
    describes; None when the rows cannot be tied to notes."""
    notes = enc.source_notes
    if enc.nested or not notes or len(notes) != len(enc.flat):
        return None
    unit = ns.numerator * 4 * ns.grid // ns.denominator
    if unit <= 0:
        return None
    first = lambda n: n[0] if isinstance(n, (list, tuple)) else n     # noqa: E731  a chord
    return [first(n).start // unit for n in notes]


def wolfram(tree) -> str:
    return render_wl(tree).replace("iC[", "IndexedConcatenate[")
