"""Find an indexed concatenation level by level, the way the Notebook is used.

concat.py looks for the single block that saves the most and repeats; what
comes out is compact but has no levels, and the rows around its blocks are
left as they were. A person in the Notebook works upward instead:

1. Level 1 cuts the rows into short pieces, each written as one €: a note
   repeated (€2[{67,16}]) or a few rows whose entries follow a polynomial in
   i (€(i,0,1)[{69-2i,32}]). Every row ends up inside a piece.
2. Each level after that takes the pieces of the level below, finds runs of
   them that repeat with the same shape -- the same kinds of €, the same
   lengths -- and writes each run as one € over a new index (j, then k, ...),
   with a formula for every number that changes from one repeat to the next.
3. That goes on until nothing groups.

How a level is cut decides what can group above it, so several cuttings are
tried and the one whose levels end in the fewest parts is kept.

Nothing is taken on two points alone unless little moves: a run of two repeats
may change at most half of its numbers, and with three repeats or more every
formula must be confirmed by a point it was not fitted on. The last repeat may
be cut short -- its missing pieces get a range that ends before it starts, so
they vanish, as in €(i,0,2-3j) -- which is how a phrase that ends early still
groups with the one before it.

Formulas are polynomials only: constant, then line, then quadratic, then
cubic. Nothing is returned unless it expands back to the input exactly.

Options (see strategies.py for the names they go by) widen what is tried:

- bars: no level-1 piece crosses a bar line, and each bar becomes one part of
  level 2 -- a € of one copy, which is the bar itself (€(i,0,0)[x] = x) -- so
  levels above group whole bars, as a phrase is heard.
- gates: a number that changes between repeats (level 2 and up; the notes of a
  level-1 piece stay plain polynomials) may also step, alternate, or
  differ in one repeat, written with a € inside the number or its count:
  67 + €(j-2)[10] is 67, 67, 67, 77; 73 - (-1)^j is 72, 74, 72, 74;
  2 + €(1-(j-2)^2)[1] is 2, 2, 3, 2. A € whose count comes out 0 or less adds
  nothing, which is what makes it a switch.
- variants: repeats that differ by one piece -- a piece missing from some, or
  extra in some -- still line up. Where it is absent its count is 0, so it
  vanishes there, and the count becomes a formula (with gates, a switch).
"""
from __future__ import annotations

import threading
from dataclasses import dataclass
from fractions import Fraction as F

import sympy

from .ic import IC, Arith, Iter, expand, render

Row = tuple
VARS = "ijklmnpqrs"      # the index of level 1, 2, 3, ...
MAX_PIECE = 12           # rows in one level-1 piece (a run may be longer)
MAX_PERIOD = 12          # pieces in one repeat
MAX_ROWS = 3000
TWO_MOVING = 1 / 3       # share of numbers a run of two repeats may change
MOVING_COST = 0.4        # a number that changes, against one more part
GATE_POINTS = 8          # repeats a gated formula is tried on, at most


@dataclass(frozen=True)
class Options:
    """What the search may do beyond the default; see the module docstring."""
    bars: tuple | None = None    # the bar of each row
    gates: bool = False
    variants: bool = False
    specials: int = 2            # gate terms one number may have
    moving_cost: float = MOVING_COST


DEFAULT = Options()


def _opts() -> Options:
    return getattr(_trace, "opts", None) or DEFAULT


class Stopped(Exception):
    """Raised from the trace callback to abandon a search part way."""


_trace = threading.local()


def _report(event: dict | None) -> None:
    cb = getattr(_trace, "cb", None)
    if cb is not None:
        cb(event)


# --------------------------------------------------------------------------
# polynomials: {monomial: coefficient}, a monomial a sorted tuple of (var, power)
# --------------------------------------------------------------------------

Poly = dict


def _const(c) -> Poly:
    return {(): F(c)} if c else {}


def _times_var(p: Poly, var: str, power: int) -> Poly:
    if not power:
        return dict(p)
    out = {}
    for m, c in p.items():
        d = dict(m)
        d[var] = d.get(var, 0) + power
        out[tuple(sorted(d.items()))] = c
    return out


def _add(p: Poly, q: Poly) -> Poly:
    out = dict(p)
    for m, c in q.items():
        out[m] = out.get(m, F(0)) + c
        if not out[m]:
            del out[m]
    return out


# A gate is a factor of a monomial named after its index: "j~h2" is
# max(0, j-2), a step after j = 2; "j~b2" is max(0, 2-j), one before it;
# "j~s2" is 1 at j = 2 and 0 elsewhere; "j~a" is (-1)^j.

def _var_of(name: str) -> str:
    return name.split("~")[0]


def _is_gate(name: str) -> bool:
    return "~" in name and name.split("~")[1][0] in "hbs"


def _gate_at(name: str, t: int) -> int:
    var, g = name.split("~")
    kind = g[0]
    if kind == "a":
        return -1 if t % 2 else 1
    c = int(g[1:])
    return max(0, t - c) if kind == "h" else max(0, c - t) if kind == "b" else int(t == c)


def _gate_count(name: str):
    """The count of a € that is this gate: it makes copies only where the gate
    is not 0, and as many as the gate's value."""
    var, g = name.split("~")
    v, c = sympy.Symbol(var), int(g[1:])
    return {"h": v - c, "b": c - v, "s": 1 - (v - c) ** 2}[g[0]]


def _uses(p: Poly, var: str) -> bool:
    return any(_var_of(v) == var for m in p for v, _ in m)


def _gates(p: Poly) -> int:
    return sum(1 for m in p for v, _ in m if "~" in v)


def _fit(vals: list[F], max_deg: int) -> list[F] | None:
    """Coefficients c0..cd of the lowest-degree polynomial in t = 0, 1, ...
    through every value, d <= max_deg; None if none fits."""
    n = len(vals)
    for d in range(0, min(max_deg, n - 1) + 1):
        # Newton forward differences fix the polynomial on the first d+1 points
        diffs, row = [], list(vals[:d + 1])
        for _ in range(d + 1):
            diffs.append(row[0])
            row = [b - a for a, b in zip(row, row[1:])]
        coef = _newton_to_power(diffs)
        if all(sum(c * t ** e for e, c in enumerate(coef)) == v for t, v in enumerate(vals)):
            return coef
    return None


def _newton_to_power(diffs: list[F]) -> list[F]:
    """Sum of diffs[k] * C(t, k) as power-basis coefficients."""
    out = [F(0)] * len(diffs)
    for k, dk in enumerate(diffs):
        # C(t, k) = t (t-1) ... (t-k+1) / k!
        poly = [F(1)]
        for r in range(k):
            poly = [F(0)] + poly                      # times t
            for e in range(len(poly) - 1):
                poly[e] -= r * poly[e + 1]            # minus r times the old poly
        fact = 1
        for r in range(2, k + 1):
            fact *= r
        for e, c in enumerate(poly):
            out[e] += dk * c / fact
    return out


def _solve(cols: list[list], vals: list) -> list | None:
    """The one x with sum(cols[c][r] * x[c]) = vals[r] for every r, or None
    when there is none or more than one. Exact, by elimination."""
    n = len(cols)
    rows = [[F(cols[c][r]) for c in range(n)] + [F(vals[r])] for r in range(len(vals))]
    piv = 0
    for c in range(n):
        at = next((r for r in range(piv, len(rows)) if rows[r][c]), None)
        if at is None:
            return None                     # this unknown is not pinned down
        rows[piv], rows[at] = rows[at], rows[piv]
        for r in range(len(rows)):
            if r != piv and rows[r][c]:
                f = rows[r][c] / rows[piv][c]
                rows[r] = [a - f * b for a, b in zip(rows[r], rows[piv])]
        piv += 1
    if any(row[-1] for row in rows[piv:]):
        return None                         # the points disagree
    return [rows[c][-1] / rows[c][c] for c in range(n)]


_GATE_FITS: dict = {}


def _fit_gated(pts: tuple, specials: int, max_base: int):
    """A constant or a line in t plus up to `specials` gate terms through every
    point (t, v): ((base coefficients), ((gate, coefficient), ...)) or None.
    Fewer gates win, then steps over one-offs over alternation. The base must
    be confirmed where every gate is 0, and the whole by a point more than it
    has unknowns."""
    key = (pts, specials, max_base)
    if key in _GATE_FITS:
        return _GATE_FITS[key]
    ts = [t for t, _ in pts]
    lo, hi = min(ts), max(ts)
    names = ([f"~h{c}" for c in range(lo, hi)] + [f"~b{c}" for c in range(lo + 1, hi + 1)]
             + [f"~s{c}" for c in ts] + ["~a"])
    at = {n: [_gate_at("x" + n, t) for t in ts] for n in names}
    plain: dict = {}            # (quiet points, degree) -> does a polynomial fit them

    def base_fits(quiet: tuple, deg: int) -> bool:
        if (quiet, deg) not in plain:
            sub = [pts[r] for r in quiet]
            plain[quiet, deg] = _solve([[t ** e for t, _ in sub] for e in range(deg + 1)],
                                       [v for _, v in sub]) is not None
        return plain[quiet, deg]

    out = None
    for g in range(1, specials + 1):
        for deg in range(0, max_base + 1):
            for combo in _combos(names, g):
                unknowns = deg + 1 + g
                if len(pts) < unknowns + 1:
                    continue
                if "~a" not in combo:
                    # where every gate is 0 the plain part alone must fit
                    quiet = tuple(r for r in range(len(ts)) if all(at[n][r] == 0 for n in combo))
                    if len(quiet) < deg + 2 or not base_fits(quiet, deg):
                        continue
                cols = [[t ** e for t in ts] for e in range(deg + 1)]
                cols += [at[n] for n in combo]
                sol = _solve(cols, [v for _, v in pts])
                if sol is None or any(c == 0 for c in sol[deg + 1:]):
                    continue
                out = (tuple(sol[:deg + 1]), tuple(zip(combo, sol[deg + 1:])))
                break
            if out:
                break
        if out:
            break
    _GATE_FITS[key] = out
    return out


def _combos(names: list, g: int):
    if g == 1:
        yield from ((n,) for n in names)
        return
    for a in range(len(names)):
        for b in range(a + 1, len(names)):
            yield (names[a], names[b])


def _fit_in(vals: list, max_deg: int, var: str, gates: bool = False) -> Poly | None:
    """A Poly in var through vals (t = 0, 1, ...; None is a value that can be
    anything). A polynomial of degree at most max_deg first; then, if gates
    are allowed and there are three points or more, a gated form."""
    pts = [(t, F(v)) for t, v in enumerate(vals) if v is not None]
    if not pts:
        return {}
    if len(pts) == len(vals):
        coef = _fit(vals, max_deg)
    else:                                   # unknown values: fit what is known
        coef = None
        for d in range(0, min(max_deg, len(pts) - 1) + 1):
            sol = _solve([[t ** e for t, _ in pts] for e in range(d + 1)], [v for _, v in pts])
            if sol is not None:
                coef = sol
                break
    if coef is not None:
        return _poly_from(coef, var)
    if not gates or not 3 <= len(pts) <= GATE_POINTS:
        return None
    got = _fit_gated(tuple(pts), _opts().specials, min(1, max_deg))
    if got is None:
        return None
    base, terms = got
    out = _poly_from(list(base), var)
    for name, c in terms:
        out = _add(out, {((var + name, 1),): F(c)})
    return out


def _poly_from(coef: list[F], var: str) -> Poly:
    out: Poly = {}
    for e, c in enumerate(coef):
        if c:
            out[((var, e),) if e else ()] = F(c)
    return out


def _sym(p: Poly):
    """A gate-free Poly as sympy; (-1)^j is written as itself."""
    out = sympy.Integer(0)
    for m, c in p.items():
        term = sympy.Rational(c.numerator, c.denominator)
        for v, e in m:
            if v.endswith("~a"):
                term *= sympy.Integer(-1) ** sympy.Symbol(_var_of(v))
            else:
                term *= sympy.Symbol(v) ** e
        out += term
    return sympy.expand(out)


def _leaf(p: Poly):
    """A number when it is one, else a sympy expression, as ic.py writes them;
    an Arith when a gate is in it: each gate a € term whose count is the gate,
    so 67 + 10 max(0, j-2) is written 67 + €(j - 2)[10]."""
    if not p:
        return 0
    if list(p) == [()]:
        c = p[()]
        return int(c) if c.denominator == 1 else c
    plain = {m: c for m, c in p.items() if not any(_is_gate(v) for v, _ in m)}
    if len(plain) == len(p):
        return _sym(p)
    by_gate: dict = {}
    for m, c in p.items():
        if m in plain:
            continue
        g = next(v for v, _ in m if _is_gate(v))
        by_gate.setdefault(g, {})[tuple(x for x in m if x[0] != g)] = c
    parts = [IC([_leaf(sub)], Iter(None, 1, _gate_count(g))) for g, sub in sorted(by_gate.items())]
    return Arith(_sym(plain) + sum(sympy.Symbol(f"_ic{k}") for k in range(len(parts))), parts)


# --------------------------------------------------------------------------
# pieces and groups
# --------------------------------------------------------------------------

@dataclass
class Seg:
    """count rows, row t = cols evaluated at var = t (t = 0 .. count-1)."""
    var: str
    count: Poly
    cols: list
    r0: int = 0
    r1: int = 0
    penalty: float = 0.0


@dataclass
class Grp:
    """kids repeated count times, var = 0 .. count-1."""
    var: str
    count: Poly
    kids: list
    r0: int = 0
    r1: int = 0
    moving: int = 0


def _sig(x):
    if isinstance(x, Seg):
        return ("S", x.var, len(x.cols))
    return ("G", x.var, tuple(_sig(k) for k in x.kids))


def _slots(x) -> list:
    if isinstance(x, Seg):
        return [x.count] + list(x.cols)
    out = [x.count]
    for k in x.kids:
        out += _slots(k)
    return out


def _rebuild(x, slots: list):
    """x's shape with these slots, in _slots order."""
    it = iter(slots)

    def go(y):
        if isinstance(y, Seg):
            return Seg(y.var, next(it), [next(it) for _ in y.cols])
        count = next(it)
        return Grp(y.var, count, [go(k) for k in y.kids])
    return go(x)


def _unit(x) -> bool:
    """A group of one copy whose index nothing uses: it is just its pieces,
    as €(i,0,0)[x] is x. Bars are held this way."""
    return (isinstance(x, Grp) and x.count == _const(1)
            and not any(_uses(s, x.var) for k in x.kids for s in _slots(k)))


def _trees(x) -> list:
    """The ic.py items for a piece or group, a one-copy group spliced."""
    if _unit(x):
        return [t for k in x.kids for t in _trees(k)]
    return [_to_tree(x)]


def _to_tree(x):
    """The ic.py tree for a piece or group."""
    if _unit(x):
        return _trees(x)
    if isinstance(x, Seg):
        row = tuple(_leaf(c) for c in x.cols)
        uses = any(_uses(c, x.var) for c in x.cols)
        if not uses and x.count == _const(1):
            return row
        if not uses:
            return IC([row], Iter(None, 1, _leaf(x.count)))
        return IC([row], Iter(x.var, 0, _leaf(_add(x.count, _const(-1))), table=True))
    kids = [t for k in x.kids for t in _trees(k)]
    if not any(_uses(s, x.var) for k in x.kids for s in _slots(k)):
        return IC(kids, Iter(None, 1, _leaf(x.count)))    # nothing varies: €(2)[...]
    return IC(kids, Iter(x.var, 0, _leaf(_add(x.count, _const(-1))), table=True))


def text(x) -> str:
    return render(_to_tree(x)).replace("**", "^")


# --------------------------------------------------------------------------
# level 1: cutting the rows into pieces
# --------------------------------------------------------------------------

def _piece(rows: list[Row], a: int, b: int) -> Seg | None:
    n = b - a
    # A formula must be confirmed by a row it was not fitted on, with two
    # exceptions people use: a line on two rows, and a curve on three whose
    # steps change by one (72, 71, 69 is 72 - i(i+1)/2).
    max_deg = 1 if n == 2 else min(n - 1, 2) if n == 3 else min(max(n - 2, 0), 3)
    opts = _opts()
    bars = opts.bars
    if bars is not None and bars[a] != bars[b - 1]:
        return None                         # a piece stays inside its bar
    cols, deg = [], 0
    for c in range(len(rows[a])):
        vals = [F(r[c]) for r in rows[a:b]]
        coef = _fit(vals, max_deg)
        if coef is None:
            return None
        if n == 3 and len(coef) == 3 and abs(vals[2] - 2 * vals[1] + vals[0]) > 1:
            return None
        cols.append(_poly_from(coef, VARS[0]))
        deg += len(coef) - 1
    # a part costs 1; formulas cost a little, a lone row a little more
    return Seg(VARS[0], _const(n), cols, a, b, 0.1 * deg + (0.3 if n == 1 else 0))


def cuttings(rows: list[Row], k: int) -> list[list[Seg]]:
    """The k cheapest ways to cut the rows into pieces."""
    n = len(rows)
    best: list[list] = [[] for _ in range(n + 1)]       # [(cost, a, idx, seg)]
    best[0] = [(0.0, -1, -1, None)]
    for b in range(1, n + 1):
        if b % 50 == 0:
            _report(None)
        opts = []
        for a in range(b - 1, -1, -1):
            if len(rows[a]) != len(rows[b - 1]):
                break
            seg = _piece(rows, a, b)
            if seg is None:
                if b - a > MAX_PIECE:
                    break
                continue
            for idx, (cost, *_rest) in enumerate(best[a]):
                opts.append((cost + 1 + seg.penalty, a, idx, seg))
            if b - a >= MAX_PIECE and any(_uses(c, VARS[0]) for c in seg.cols):
                break                   # only a run goes on past MAX_PIECE
        opts.sort(key=lambda o: o[0])
        best[b] = opts[:k]
    out = []
    for idx in range(len(best[n])):
        segs, b, i = [], n, idx
        while b > 0:
            _cost, a, j, seg = best[b][i]
            segs.append(seg)
            b, i = a, j
        out.append(segs[::-1])
    return out


# --------------------------------------------------------------------------
# levels 2, 3, ...: grouping pieces that repeat with the same shape
# --------------------------------------------------------------------------

def _block(items: list, s: int, p: int, q: int, m: int, var: str):
    """Items s .. as q repeats of p, the last cut to its first m (m = p: whole).
    Returns (Grp, moving, total) or a reason it fails."""
    periods = [items[s + t * p: s + t * p + p] for t in range(q - 1)]
    last = items[s + (q - 1) * p: s + (q - 1) * p + m]
    if len(last) != m:
        return "short"
    if m < p:
        # the missing pieces vanish: the one before, with nothing to count
        last = last + [_rebuild(x, [{}] + _slots(x)[1:]) for x in periods[-1][m:]]
    periods.append(last)
    got = _fit_periods(periods, var)
    if isinstance(got, str):
        return got
    kids, moving, total = got
    first, end = items[s], items[s + (q - 1) * p + m - 1]
    return Grp(var, _const(q), kids, first.r0, end.r1, moving), moving, total


def _fit_periods(periods: list[list], var: str):
    """One body for periods of items lined up position by position: every
    number of every item fitted as a formula in var. A slot that is None (in a
    piece absent from that period) can be anything. Returns (kids, moving,
    total) -- moving counts numbers that change, a gate one more -- or why
    it fails."""
    q, p = len(periods), len(periods[0])
    for x in range(p):
        if len({_sig(per[x]) for per in periods}) != 1:
            return "shape"
    gates = _opts().gates
    kids, moving, total = [], 0, 0
    for x in range(p):
        cols = [_slots(per[x]) for per in periods]
        new = []
        for sl in range(len(cols[0])):
            known = sum(1 for c in cols if c[sl] is not None)
            max_deg = 1 if known <= 2 else min(known - 2, 2)
            monos = sorted({mo for c in cols if c[sl] is not None for mo in c[sl]})
            acc: Poly = {}
            for mo in monos:
                vals = [None if c[sl] is None else c[sl].get(mo, F(0)) for c in cols]
                poly = _fit_in(vals, max_deg, var, gates)
                if poly is None:
                    return "nofit"
                total += 1
                if _uses(poly, var):
                    moving += 1 + _gates(poly)
                acc = _add(acc, _times_monomial(poly, mo))
            if not monos:
                total += 1
            new.append(acc)
        base = next(per[x] for per in periods if not _phantom_of(per[x]))
        kids.append(_rebuild(base, new))
    if q == 2 and moving > TWO_MOVING * total:
        return "loose"
    return kids, moving, total


def _phantom(x):
    """x's shape with nothing in it: no copies, and every other number free."""
    n = len(_slots(x))
    return _rebuild(x, [{}] + [None] * (n - 1))


def _phantom_of(x) -> bool:
    return _slots(x)[1:2] == [None]


def _times_monomial(p: Poly, mono) -> Poly:
    out = p
    for v, e in mono:
        out = _times_var(out, v, e)
    return out


def _level(items: list, var: str, report: bool = False) -> list | None:
    """The cheapest grouping of one level's items; None if nothing groups."""
    opts = _opts()
    cost_of = lambda mv: 1 + opts.moving_cost * mv          # noqa: E731
    L = len(items)
    found: list[list] = [[] for _ in range(L)]         # by start: (end, Grp, cost)
    for s in range(L):
        _report(None)
        for p in range(1, min(MAX_PERIOD, (L - s) // 2) + 1):
            q, failed = 2, None
            while s + q * p <= L:
                got = _block(items, s, p, q, p, var)
                if isinstance(got, str):
                    if report and got == "loose":
                        _report({"t": "try", "r0": items[s].r0,
                                 "r1": items[s + q * p - 1].r1, "p": p, "q": q,
                                 "status": "loose"})
                    failed = failed or q
                    if opts.gates and got in ("nofit", "loose") and q < GATE_POINTS:
                        q += 1          # a gate is only taken on three repeats or more
                        continue
                    break
                g, mv, _tot = got
                found[s].append((s + q * p, g, cost_of(mv)))
                q += 1
            q = failed or q
            # the last repeat cut short
            qq = max(2, q - 1) if q > 2 else 2
            # it must keep most of the phrase: at most two pieces missing
            for m in range(max((p + 1) // 2, p - 2, 1), p):
                if s + (qq - 1) * p + m > L:
                    break
                got = _block(items, s, p, qq, m, var)
                if isinstance(got, str):
                    continue
                g, mv, _tot = got
                found[s].append((s + (qq - 1) * p + m, g, cost_of(mv) + 0.05))
        if opts.variants:
            for p in range(1, min(MAX_PERIOD, L - s) + 1):
                for end, g, mv, ghosts in _variants(items, s, p, var):
                    found[s].append((end, g, cost_of(mv) + 0.05 * ghosts))
    # cheapest cover: an item alone costs 1
    INF = float("inf")
    cost = [INF] * (L + 1)
    back: list = [None] * (L + 1)
    cost[0] = 0.0
    for s in range(L):
        if cost[s] == INF:
            continue
        if cost[s] + 1 < cost[s + 1]:
            cost[s + 1], back[s + 1] = cost[s] + 1, (s, None)
        for end, g, c in found[s]:
            if cost[s] + c < cost[end]:
                cost[end], back[end] = cost[s] + c, (s, g)
    out, chosen, e = [], set(), L
    while e > 0:
        s, g = back[e]
        out.append(g if g is not None else items[s])
        if g is not None:
            chosen.add(id(g))
        e = s
    out.reverse()
    if not chosen:
        return None
    if report:
        for s in range(L):
            for end, g, _c in found[s]:
                kept = id(g) in chosen
                _report({"t": "try", "r0": g.r0, "r1": g.r1,
                         "p": len(g.kids), "q": int(g.count[()]),
                         "status": "best" if kept else "beaten",
                         "text": text(g) if kept else None})
    return out


def _variants(items: list, s: int, p: int, var: str):
    """Repeats of items[s:s+p] where some repeat lacks one piece or has one
    more, at the same place each time. A piece absent from a repeat is put
    there with no copies, so every repeat has the same shape and the count
    of that piece is fitted like any other number. Yields (end, Grp, moving,
    pieces put in) for every run of two repeats or more with a variant in it."""
    sig = [_sig(x) for x in items[s:s + p]]
    L, pos = len(items), s + p
    runs = [("same", items[s:s + p], None)]
    extra_at, extra_sig = None, None
    while pos < L:
        if [_sig(x) for x in items[pos:pos + p]] == sig:
            runs.append(("same", items[pos:pos + p], None)); pos += p
            continue
        took = False
        if pos + p + 1 <= L:
            cand = items[pos:pos + p + 1]
            for x in ([extra_at] if extra_at is not None else range(p + 1)):
                rest = cand[:x] + cand[x + 1:]
                if ([_sig(y) for y in rest] == sig
                        and extra_sig in (None, _sig(cand[x]))):
                    extra_at, extra_sig = x, _sig(cand[x])
                    runs.append(("extra", cand, x)); pos += p + 1; took = True
                    break
        if not took and p >= 2 and pos + p - 1 <= L:
            cand = items[pos:pos + p - 1]
            for x in range(p):
                if [_sig(y) for y in cand] == sig[:x] + sig[x + 1:]:
                    runs.append(("missing", cand, x)); pos += p - 1; took = True
                    break
        if not took:
            break
    if len(runs) < 2 or all(kind == "same" for kind, _, _ in runs):
        return
    ends = [s]
    for _kind, cand, _x in runs:
        ends.append(ends[-1] + len(cand))
    # A piece absent from some repeats has a count of 0 there and 1 elsewhere,
    # which takes a gate, and gates are tried on GATE_POINTS repeats at most;
    # a few shorter runs are tried if the longest does not fit.
    misses = 0
    for q in range(min(len(runs), GATE_POINTS), 1, -1):
        if misses >= 3:
            return
        use = runs[:q]
        if all(kind == "same" for kind, _, _ in use):
            break
        # one shape for every repeat: the extra piece's place, then each missing one
        extra = next((c[x] for kind, c, x in use if kind == "extra"), None)
        aligned, ghosts = [], 0
        for kind, cand, x in use:
            per = list(cand)
            if kind == "missing":
                per.insert(x, _phantom(items[s + x])); ghosts += 1
            if kind != "extra" and extra is not None:
                per.insert(extra_at, _phantom(extra)); ghosts += 1
            aligned.append(per)
        if any(all(_phantom_of(per[x]) for per in aligned) for x in range(len(aligned[0]))):
            continue
        got = _fit_periods(aligned, var)
        if isinstance(got, str):
            misses += 1
            continue
        kids, moving, _total = got
        real = [y for _kind, c, _ in use for y in c]
        yield (ends[q], Grp(var, _const(q), kids, real[0].r0, real[-1].r1, moving),
               moving, ghosts)


def _climb(segs: list[Seg], report: bool = False) -> list[list]:
    """Every level from a cutting: [level-1 pieces, level-2 parts, ...]."""
    levels = [segs]
    first, bars = 1, _opts().bars
    if bars is not None:
        # level 2 is the bars: each a group of one copy, so bars of the same
        # shape can line up above it
        by_bar: dict = {}
        for sg in segs:
            by_bar.setdefault(bars[sg.r0], []).append(sg)
        levels.append([Grp(VARS[1], _const(1), kids, kids[0].r0, kids[-1].r1)
                       for _b, kids in sorted(by_bar.items())])
        first = 2
        if report:
            _report({"t": "round", "n": 2, "r0": 0, "r1": segs[-1].r1,
                     "label": f"Level 2: {len(segs)} pieces into {len(levels[-1])} bars"})
    for depth in range(first, len(VARS)):
        if report:
            _report({"t": "round", "n": depth + 1, "r0": 0, "r1": segs[-1].r1,
                     "label": f"Level {depth + 1}: grouping {len(levels[-1])} "
                              f"{'pieces' if depth == 1 else 'parts'}"})
        nxt = _level(levels[-1], VARS[depth], report)
        if nxt is None:
            if report:
                _report({"t": "none", "r0": 0, "r1": segs[-1].r1,
                         "why": "nothing groups at this level, so the level below is the answer"})
            break
        if report:
            below = {id(x) for x in levels[-1]}
            for g in nxt:
                if id(g) not in below:
                    _report({"t": "keep", "r0": g.r0, "r1": g.r1, "text": text(g),
                             "saving": None})
        levels.append(nxt)
    return levels


def _score(levels: list[list]) -> float:
    top = levels[-1]
    pen = sum(s.penalty for s in levels[0])
    moving = sum(g.moving for lv in levels[1:] for g in lv if isinstance(g, Grp))
    return len(top) + 0.1 * pen + _opts().moving_cost * moving


# --------------------------------------------------------------------------
# the search
# --------------------------------------------------------------------------

@dataclass
class Result:
    levels: list            # per level, its parts (Seg or Grp)
    rows: list
    verified: bool

    @property
    def tree(self) -> list:
        return [t for x in self.levels[-1] for t in _trees(x)]

    @property
    def text(self) -> str:
        return render(self.tree).replace("**", "^")

    def level_parts(self) -> list[list[dict]]:
        return [[{"text": text(x), "r0": x.r0, "r1": x.r1,
                  "kind": "group" if isinstance(x, Grp) else "piece"} for x in lv]
                for lv in self.levels]


def find(rows, on_event=None, opts: Options | None = None) -> Result:
    """Concatenate rows level by level. With `on_event`, the search reports
    as it goes, in the events concat.concatenate uses, plus a "label" on each
    round; it is also called with None often, so raising Stopped is prompt.
    `opts` widens the search (see Options); the default is the search the
    Notebook has always used."""
    rows = [tuple(int(x) for x in r) for r in rows]
    if not rows:
        return Result([[]], rows, True)
    opts = opts or DEFAULT
    if opts.bars is not None and len(opts.bars) != len(rows):
        raise ValueError(f"{len(opts.bars)} bar numbers for {len(rows)} rows")
    _trace.cb, _trace.opts = on_event, opts
    try:
        n = len(rows)
        k = 24 if n <= 120 else 8 if n <= 600 else 3
        _report({"t": "round", "n": 1, "r0": 0, "r1": n,
                 "label": f"Level 1: cutting {n} rows into pieces, {k} ways tried"})
        best, best_score = None, None
        if n <= MAX_ROWS:
            for w, segs in enumerate(cuttings(rows, k), 1):
                levels = _climb(segs)
                sc = _score(levels)
                better = best_score is None or sc < best_score
                if better:
                    best, best_score = levels, sc
                _report({"t": "try", "r0": 0, "r1": n, "status": "best" if better else "beaten",
                         "cut": w, "pieces": len(segs), "parts": len(levels[-1]),
                         "levels": len(levels)})
        if best is None:                # too many rows: each row its own piece
            best = [[Seg(VARS[0], _const(1), [_const(v) for v in r], t, t + 1)
                     for t, r in enumerate(rows)]]
        else:
            # tell the chosen cutting's story, level by level
            _report({"t": "keep", "r0": 0, "r1": n, "saving": None,
                     "text": f"{len(best[0])} pieces: " + ", ".join(text(s) for s in best[0])})
            best = _climb(best[0], report=True)
        res = Result(best, rows, False)
        res.verified = [tuple(x) for x in expand(res.tree)] == rows
        return res
    finally:
        _trace.cb = _trace.opts = None
