"""Find an indexed concatenation for a list of rows.

A rewrite of what ReduceSetList (SSSiCv102.wl) does, aiming to compress more.
The Wolfram code reduces greedily: the shortest repeating pattern anywhere is
fixed first, and a later, larger block can be blocked by it (example 3 in
docs/concatenation-examples.md). Here every candidate block is scored by how
many written leaves it saves, the best one wins, and what is left on either
side is reduced again from the raw rows -- so nothing chosen early can get in
the way of a block found later.

The steps:

1. Segment the rows into *atoms*. An atom is a row repeated n times (a run,
   n >= 1) or an arithmetic progression of rows (row a, step d, length n).
   A run is just a progression with step 0, so both are (a, d, n).
2. Look for periods: p consecutive atoms whose row lengths repeat. Every
   number in a period -- the entries of a and d, and the count n -- becomes a
   column, and each column is fitted as a function of the period number k
   (constant, linear, quadratic, c*b^k plus a polynomial). A single row has no
   step, so its d is a wildcard: that is what lets a period with one copy line
   up with periods that have a run or a progression in the same place.
3. Extend the best candidates at the row level. The body is evaluated at
   k = 0, -1, ... and at k = q+1, ... and kept wherever it reproduces the
   neighbouring rows exactly. A count of 0 or 1 is legal there, so periods the
   atom segmentation could not line up (a missing run, a run of one) are
   absorbed. The body is also tried at every rotation, which decides where a
   period starts.
4. Keep the block that saves the most, reindex it to run from k = 1, and
   reduce the rows before and after it the same way.

That finds one level of blocks over atoms. The networks of the ICN paper
(Indexed-Concatenation-Notation-Extended.2.nb, sections 2, 7 and 8) need
blocks of blocks, so a token layer then looks for more (see "tokens" below):

5. Cut the rows into tokens three ways -- the items of the result of 1-4,
   exactly repeated groups of rows (€k[{1,1},{1,2k}] for one k), and pieces
   that each start with one kind of row, reduced on their own -- and look for
   periods over the tokens as 2-3 do over atoms: every count, limit and
   coefficient is a column fitted in a new outer index, and the periods are
   extended at the row level, which absorbs the first periods where an inner
   block has a count of 0 or 1.
6. Search the body of every block again, at every depth (€(j,1,4)[...] in
   Fig. 3's body, €2[{1,2},€2[{}]] in Fig. 5(e)), and pair stretches that
   differ only by extra pieces, written under €(m) for m = 0, 1 (Fig. 4(b)).
7. Write each index from where its body reads simplest, and a run of equal
   numbers inside a row as one (€(j,k,6)[{j}], {1,€3[3]}).

Of all these forms the one with the fewest leaves, then the fewest numbers
written, is kept. Nothing is returned unless it expands back to the input
exactly.
"""
from __future__ import annotations

import functools
import re
import threading
from dataclasses import dataclass
from fractions import Fraction as F

import sympy

from .ic import IC, Iter, Seq, expand, render

Row = tuple

K = sympy.Symbol("k")
J = sympy.Symbol("j")

MAX_PERIOD = 64        # atoms in one period
MAX_ROWS = 20000
EXTEND_TOP = 24        # candidates taken to the row-level extension step


# --------------------------------------------------------------------------
# tracing: what the search tries, for anyone watching it run
# --------------------------------------------------------------------------

class Stopped(Exception):
    """Raised from the trace callback to abandon a search part way."""


_trace = threading.local()


def _report(event: dict) -> None:
    """Hand an event to the caller's callback, if it asked for them. Row
    numbers are made absolute: the search works on slices of the rows."""
    cb = getattr(_trace, "cb", None)
    if cb is None:
        return
    base = _trace.base
    for k in ("r0", "r1"):
        if k in event:
            event[k] += base
    cb(event)


def _tick() -> None:
    """A chance to stop between reports: the callback gets None."""
    cb = getattr(_trace, "cb", None)
    if cb is not None:
        cb(None)


# --------------------------------------------------------------------------
# formulas: sums of basis functions of k with exact rational coefficients
# --------------------------------------------------------------------------

BASES = (2, 3, 4, 5, 10)


def _basis_value(b, k: int) -> F:
    if b == "1":
        return F(1)
    if b == "k":
        return F(k)
    if b == "k2":
        return F(k * k)
    return F(b) ** k                       # an exponential base


def _models(loose: bool = False):
    """Candidate models, simplest first, with how many points beyond the ones
    that determine it must also check out. A line through two points is
    accepted the way FindSequenceFunction accepts it; everything richer has to
    predict a point it was not fitted on. `loose` drops that requirement, for
    deciding whether a block might still continue."""
    x = 0 if loose else 1
    # polynomials first: 72, 71, 69 is 72 - k(k-1)/2 before it is 73 - 2^k/2
    yield ("1",), 0
    yield ("1", "k"), 0
    yield ("1", "k", "k2"), x
    # the base of a power is picked to fit as well, so it needs a point more
    for b in BASES:
        yield ("1", b), x + 1
    for b in BASES:
        yield ("1", "k", b), x + 1


@dataclass(frozen=True)
class Formula:
    terms: tuple            # ((basis, coefficient), ...)

    def __post_init__(self):
        # integer coefficients, when they all are: evaluated without Fractions
        ints = None
        if all(c.denominator == 1 for _, c in self.terms):
            ints = tuple((b, int(c)) for b, c in self.terms)
        object.__setattr__(self, "_ints", ints)

    def __call__(self, k: int) -> F | int:
        # called millions of times while blocks grow: plain int arithmetic
        # whenever it can be (an int compares and computes like a Fraction)
        ints = self._ints
        if ints is not None and k >= 0:
            total = 0
            for b, c in ints:
                if b == "1":
                    total += c
                elif b == "k":
                    total += c * k
                elif b == "k2":
                    total += c * k * k
                else:
                    total += c * b ** k
            return total
        total = F(0)
        for b, c in self.terms:
            if b == "1":
                total += c
            elif b == "k":
                total += c * k
            elif b == "k2":
                total += c * (k * k)
            else:
                total += c * (b ** k if k >= 0 else F(1, b ** -k))
        return total

    def sym(self, var=K):
        out = sympy.Integer(0)
        for b, c in self.terms:
            coef = sympy.Rational(c.numerator, c.denominator)
            if b == "1":
                out += coef
            elif b == "k":
                out += coef * var
            elif b == "k2":
                out += coef * var ** 2
            else:
                out += coef * sympy.Integer(b) ** var
        return sympy.expand(out)

    @property
    def is_const(self) -> bool:
        return all(b == "1" for b, _ in self.terms)

    @property
    def cost(self) -> int:
        """How hard the formula is to read: a term per basis function, one
        more for a coefficient other than +-1."""
        return sum(1 if abs(c) == 1 else 2 for _, c in self.terms)

    @property
    def plain_line(self) -> bool:
        """Constant, or a step of +-1: the only lines two points are trusted
        to give."""
        return all(b == "1" or (b == "k" and abs(c) == 1) for b, c in self.terms)

    def const(self):
        return self(0) if self.is_const else None


def _solve(basis, pts) -> Formula | None:
    """Coefficients making the basis pass through the first len(basis) points."""
    n = len(basis)
    rows = [[_basis_value(b, k) for b in basis] + [v] for k, v in pts[:n]]
    for col in range(n):
        piv = next((r for r in range(col, n) if rows[r][col] != 0), None)
        if piv is None:
            return None
        rows[col], rows[piv] = rows[piv], rows[col]
        pv = rows[col][col]
        rows[col] = [x / pv for x in rows[col]]
        for r in range(n):
            if r != col and rows[r][col] != 0:
                f = rows[r][col]
                rows[r] = [x - f * y for x, y in zip(rows[r], rows[col])]
    terms = tuple((b, rows[i][n]) for i, b in enumerate(basis) if rows[i][n] != 0)
    return Formula(terms or (("1", F(0)),))


def fit(pts: list[tuple[int, F]], loose: bool = False) -> Formula | None:
    """The simplest model through every (k, value) point, or None."""
    if not pts:
        return None
    return _fit(tuple(pts), loose)


@functools.lru_cache(maxsize=1 << 18)
def _fit(pts: tuple, loose: bool) -> Formula | None:
    # The search refits the same few columns over and over -- every phase of
    # every period length grows through them again -- so the answers are kept.
    for basis, extra in _models(loose):
        if len(pts) < len(basis) + extra:
            continue
        f = _solve(basis, pts)
        if f is not None and all(f(k) == v for k, v in pts):
            return f
    return None


class Column:
    """Points arriving one period at a time, refitted only when the current
    formula stops predicting them."""
    __slots__ = ("pts", "f", "loose")

    def __init__(self, loose: bool = False):
        self.pts: list[tuple[int, F]] = []
        self.f: Formula | None = None
        self.loose = loose

    def add(self, k: int, v) -> bool:
        if v is None:
            return True
        self.pts.append((k, v))
        if self.f is not None and self.f(k) == v:
            return True
        self.f = fit(self.pts, self.loose)
        return self.f is not None


# --------------------------------------------------------------------------
# atoms
# --------------------------------------------------------------------------

@dataclass(frozen=True)
class Atom:
    a: Row
    d: Row | None          # None for a single row: its step is a wildcard
    n: int

    @property
    def width(self) -> int:
        return len(self.a)


def _diff(x: Row, y: Row) -> Row:
    return tuple(q - p for p, q in zip(x, y))


def segment(rows: list[Row], prog_min: int | None) -> list[Atom]:
    """Runs of equal rows first; then, among single rows, progressions of at
    least prog_min rows with a constant nonzero step (None: no progressions)."""
    out: list[Atom] = []
    i, L = 0, len(rows)
    while i < L:
        r = i
        while r + 1 < L and rows[r + 1] == rows[i]:
            r += 1
        if r > i:
            out.append(Atom(rows[i], (0,) * len(rows[i]), r - i + 1))
            i = r + 1
            continue
        if prog_min:
            e = i
            step = None
            while e + 1 < L and len(rows[e + 1]) == len(rows[i]):
                # a row that starts a run is left to that run
                if e + 2 < L and rows[e + 2] == rows[e + 1]:
                    break
                s = _diff(rows[e], rows[e + 1])
                if step is None:
                    if not any(s):
                        break
                    step = s
                elif s != step:
                    break
                e += 1
            if e - i + 1 >= prog_min:
                out.append(Atom(rows[i], step, e - i + 1))
                i = e + 1
                continue
        out.append(Atom(rows[i], None, 1))
        i += 1
    return out


def _rle_leaves(rows: list[Row]) -> int:
    """Leaves written when only runs are collapsed: the baseline to beat."""
    return sum(1 for i, r in enumerate(rows) if i == 0 or rows[i - 1] != r)


# --------------------------------------------------------------------------
# periodic blocks over the atoms
# --------------------------------------------------------------------------

@dataclass
class Body:
    """One period: per position, formulas for a, d and n as functions of k."""
    a: list[list[Formula]]
    d: list[list[Formula | None]]
    n: list[Formula]

    @property
    def p(self) -> int:
        return len(self.n)

    @property
    def cost(self) -> int:
        fs = [f for row in self.a + self.d for f in row if f is not None] + self.n
        return sum(f.cost for f in fs)

    def rows_at(self, i: int, k: int) -> list[Row] | None:
        """The rows position i expands to at period k, or None if invalid."""
        n = self.n[i](k)
        if n.denominator != 1 or n < 0:
            return None
        a = [f(k) for f in self.a[i]]
        if any(x.denominator != 1 for x in a):
            return None
        if n <= 1:
            return [tuple(int(x) for x in a)] * int(n)
        d = [f(k) if f is not None else F(0) for f in self.d[i]]
        if any(x.denominator != 1 for x in d):
            return None
        return [tuple(int(x + y * j) for x, y in zip(a, d)) for j in range(int(n))]


def _columns_for(atoms: list[Atom], s: int, p: int, loose: bool = False):
    w = [atoms[s + i].width for i in range(p)]
    return ([[Column(loose) for _ in range(wi)] for wi in w],
            [[Column(loose) for _ in range(wi)] for wi in w],
            [Column(loose) for _ in range(p)], w)


def _feed(cols, atom: Atom, i: int, k: int) -> bool:
    ca, cd, cn, _ = cols
    for c, v in zip(ca[i], atom.a):
        if not c.add(k, v):
            return False
    if atom.d is not None and atom.n > 1:
        for c, v in zip(cd[i], atom.d):
            if not c.add(k, v):
                return False
    return cn[i].add(k, atom.n)


def _grow(atoms: list[Atom], s: int, p: int) -> int:
    """How many whole periods from atom s share widths and could still be
    fitted. Loose: a formula that needs one more period to be confirmed does
    not stop the growth; _body confirms it afterwards."""
    # The first period always fits (one point is a constant), so when the
    # second does not line up by widths the answer is 1 without fitting.
    if s + 2 * p <= len(atoms) and any(
            atoms[s + p + i].width != atoms[s + i].width for i in range(p)):
        return 1
    cols = _columns_for(atoms, s, p, loose=True)
    w = cols[3]
    q = 0
    while s + (q + 1) * p <= len(atoms):
        base = s + q * p
        if any(atoms[base + i].width != w[i] for i in range(p)):
            break
        if not all(_feed(cols, atoms[base + i], i, q + 1) for i in range(p)):
            break
        q += 1
    return q


# A block of only two periods is taken when at most this share of its entries
# move between them; the rest must stay exactly the same. Two points fit any
# line, so a pair of periods that differ everywhere is a coincidence, not a
# pattern -- but a phrase repeated with a note or two changed is the commonest
# shape in music.
TWO_PERIOD_MOVING = 1 / 4


def _body(atoms: list[Atom], s: int, p: int, q: int, two: bool = False) -> Body | None:
    """The formulas for q periods from atom s, each confirmed on all its points.
    With `two` (and q = 2), lines through two points are allowed as long as
    few entries move: see TWO_PERIOD_MOVING."""
    cols = _columns_for(atoms, s, p, loose=True)
    for t in range(q):
        for i in range(p):
            if not _feed(cols, atoms[s + t * p + i], i, t + 1):
                return None
    ca, cd, cn, _ = cols
    every = [c for row in ca + cd for c in row] + cn
    for c in every:
        if c.pts and (c.f is None or c.f.is_const is False):
            c.f = fit(c.pts)
            if c.f is None:
                return None
    # Two points fix any line, so a line through only two says nothing unless
    # it is the plain step the notebooks use (4 - n, 2 + n).
    loose = [c for c in every if len(c.pts) == 2 and not c.f.plain_line]
    if loose:
        if not two or q != 2:
            return None
        values = [c for row in ca for c in row if c.pts]
        moving = [c for c in values if not c.f.is_const]
        if any(not c.f.is_const for c in cn) or \
                len(moving) > TWO_PERIOD_MOVING * len(values):
            return None
    return Body([[c.f for c in row] for row in ca],
                [[c.f for c in row] for row in cd],
                [c.f for c in cn])


def _candidates(atoms: list[Atom]):
    """(saving, s, p, q) for every periodic stretch worth considering."""
    m = len(atoms)
    found = []
    for p in range(1, min(MAX_PERIOD, m // 2) + 1):
        for phase in range(p):
            s = phase
            while s + 2 * p <= m:
                _tick()
                q = _grow(atoms, s, p)
                if q >= 2:
                    found.append(((q - 1) * p, s, p, q))
                    # the first period may be the irregular one: try one later
                    q2 = _grow(atoms, s + p, p) if s + 3 * p <= m else 0
                    if q2 >= q:
                        found.append(((q2 - 1) * p, s + p, p, q2))
                # Growth stopped where a period broke the fit. That period may
                # start the next block, and so may the one before it: early
                # points can steer a loose fit wrong. Resume from there.
                s += max(1, q - 1) * p
    found.sort(key=lambda c: (-c[0], c[2]))
    return found


# --------------------------------------------------------------------------
# row-level extension, rotation and reindexing
# --------------------------------------------------------------------------

def _period_rows(body: Body, r: int, t: int) -> list[Row] | None:
    """Rows of virtual period t when the body is rotated by r: the last r
    positions taken from period t-1, then the first p-r from period t."""
    p = body.p
    order = [(i, t - 1) for i in range(p - r, p)] + [(i, t) for i in range(p - r)]
    out: list[Row] = []
    for i, k in order:
        got = body.rows_at(i, k)
        if got is None:
            return None
        out.extend(got)
    return out


def _extend(rows, body: Body, r: int, lo: int, hi: int, r0: int, r1: int):
    """Grow the virtual-period range [lo, hi] and row span [r0, r1) outward
    while the body keeps reproducing the neighbouring rows."""
    for _ in range(len(rows)):
        got = _period_rows(body, r, lo - 1)
        if not got or r0 - len(got) < 0 or rows[r0 - len(got):r0] != got:
            break
        lo, r0 = lo - 1, r0 - len(got)
    for _ in range(len(rows)):
        got = _period_rows(body, r, hi + 1)
        if not got or rows[r1:r1 + len(got)] != got:
            break
        hi, r1 = hi + 1, r1 + len(got)
    return lo, hi, r0, r1


def _rebase(body: Body, r: int, lo: int, hi: int) -> Body | None:
    """The rotated body refitted so that its index runs k = 1 .. hi-lo+1."""
    p = body.p
    order = [(i, -1) for i in range(p - r, p)] + [(i, 0) for i in range(p - r)]
    ks = range(1, hi - lo + 2)

    def refit(f):
        if f is None:
            return None
        return fit([(k, f(k + lo - 1 + shift)) for k in ks])

    a, d, n = [], [], []
    for i, shift in order:
        a.append([refit(f) for f in body.a[i]])
        d.append([refit(f) for f in body.d[i]])
        n.append(refit(body.n[i]))
        if any(f is None for f in a[-1]) or n[-1] is None:
            return None
    return Body(a, d, n)


@dataclass
class Block:
    r0: int
    r1: int
    body: Body
    q: int
    saving: int             # leaves saved against collapsing runs only

    @property
    def rank(self):
        """More leaves saved first; among equals, the easier formulas."""
        return (self.saving, -self.body.cost)


def _best_block(rows: list[Row]) -> Block | None:
    best: Block | None = None
    # A progression of two rows is barely a pattern and tends to pair rows
    # that only happen to differ evenly; the row-level extension still covers
    # a period whose progression is that short.
    for pass_no, prog_min in enumerate((3, None), 1):
        atoms = segment(rows, prog_min)
        off = [0]
        for at in atoms:
            off.append(off[-1] + at.n)
        for _, s, p, q in _candidates(atoms)[:EXTEND_TOP]:
            body, q0 = None, q
            while q >= 2 and (body := _body(atoms, s, p, q)) is None:
                q -= 1          # the last period was only loosely fitted
            if body is None and q0 >= 2:
                q = 2           # a phrase stated twice, with a few notes moved
                body = _body(atoms, s, p, 2, two=True)
            if body is None:
                _report({"t": "try", "pass": pass_no, "r0": off[s],
                         "r1": off[s + max(q, 2) * p], "p": p, "status": "nofit"})
                continue
            # what this candidate came to, over all its rotations, for the trace
            seen: dict = {"status": "short", "r0": off[s], "r1": off[s + q * p], "q": q}
            for r in range(p):
                _tick()
                if r == 0:
                    lo, hi, r0, r1 = 1, q, off[s], off[s + q * p]
                else:
                    # periods 2..q of the rotated body sit exactly on atoms
                    lo, hi = 2, q
                    r0, r1 = off[s + p - r], off[s + q * p - r]
                lo, hi, r0, r1 = _extend(rows, body, r, lo, hi, r0, r1)
                if hi - lo + 1 < 2:
                    continue
                saving = _rle_leaves(rows[r0:r1]) - p
                here = {"r0": r0, "r1": r1, "q": hi - lo + 1, "saving": saving}
                if best is not None and saving < best.saving:
                    if seen["status"] in ("short", "refit") or \
                            (seen["status"] == "beaten" and saving > seen["saving"]):
                        seen = {**here, "status": "beaten", "best": best.saving}
                    continue
                rb = _rebase(body, r, lo, hi)
                if rb is None:
                    if seen["status"] == "short":
                        seen = {**here, "status": "refit"}
                    continue
                cand = Block(r0, r1, rb, hi - lo + 1, saving)
                if best is None or cand.rank > best.rank:
                    best = cand
                    seen = {**here, "status": "best", "rotation": r,
                            "text": render(_block_tree(cand)).replace("**", "^")}
                elif seen["status"] != "best":
                    seen = {**here, "status": "beaten", "best": best.saving}
            _report({"t": "try", "pass": pass_no, "p": p, **seen})
    return best if best is not None and best.saving > 0 else None


# --------------------------------------------------------------------------
# building the tree
# --------------------------------------------------------------------------

def _rle_tree(rows: list[Row]) -> list:
    out: list = []
    i = 0
    while i < len(rows):
        r = i
        while r + 1 < len(rows) and rows[r + 1] == rows[i]:
            r += 1
        n = r - i + 1
        out.append(rows[i] if n == 1 else IC([rows[i]], Iter(None, 1, n)))
        i = r + 1
    return out


def _num(f: Formula):
    c = f.const()
    if c is not None and c.denominator == 1:
        return int(c)
    return f.sym()


def _block_tree(block: Block) -> IC:
    b = block.body
    items: list = []
    for i in range(b.p):
        n = b.n[i]
        a = [_num(f) for f in b.a[i]]
        if n.is_const and n.const() == 1:
            items.append(tuple(a))
            continue
        steps = [f for f in b.d[i]]
        if all(f is None or (f.is_const and f.const() == 0) for f in steps):
            items.append(IC([tuple(a)], Iter(None, 1, _num(n))))
            continue
        row = tuple(
            _num(fa) if fd is None or (fd.is_const and fd.const() == 0)
            else sympy.expand(fa.sym() + fd.sym() * (J - 1))
            for fa, fd in zip(b.a[i], steps))
        items.append(IC([row], Iter("j", 1, _num(n), table=True)))
    return IC(items, Iter("k", 1, block.q, table=True))


def _reduce(rows: list[Row], base: int = 0) -> list:
    if len(rows) < 2:
        return list(rows)
    _trace.base = base
    _trace.round = getattr(_trace, "round", 0) + 1
    _report({"t": "round", "n": _trace.round, "r0": 0, "r1": len(rows)})
    block = _best_block(rows)
    _trace.base = base
    if block is None:
        _report({"t": "none", "r0": 0, "r1": len(rows)})
        return _rle_tree(rows)
    tree = _block_tree(block)
    _report({"t": "keep", "r0": block.r0, "r1": block.r1, "saving": block.saving,
             "text": render(tree).replace("**", "^")})
    return (_reduce(rows[:block.r0], base) + [tree]
            + _reduce(rows[block.r1:], base + block.r1))


def leaves(tree) -> int:
    """Rows written down to state the expression: a row is 1, an IC is the
    leaves of its body."""
    return sum(leaves(x.args) if isinstance(x, IC) else 1 for x in tree)


def written(tree) -> int:
    """Numbers and index names written down, iterators included: the second
    measure of size, deciding between forms with the same leaves."""
    return len(re.findall(r"\d+|[A-Za-z]\w*", render(tree)))


# --------------------------------------------------------------------------
# tokens: rows and blocks as items whose numbers can vary
# --------------------------------------------------------------------------
#
# The search above lines up atoms -- a row, a run, a progression. The networks
# of the ICN paper need more: a period may hold a block whose body is several
# rows (Fig. 6(b), €k[{1,1},{1,2k}]), or a block of blocks (Fig. 6(d),
# €n[€(i,1,n)[€i[...]]]), and a block's body may itself repeat (Fig. 3,
# €(k,1,4)[€n[{1}], {1,5n+5+k}]). A token is any item -- a row, or a block
# nested to any depth -- with each number written as a sum of terms c * key.
# A key is a product of factors index^e and b^index (the empty product is 1)
# and c is rational. Tokens with the same keys in the same places have the
# same *shape*; their coefficients are the *parameters*, and periods of tokens
# are fitted parameter by parameter, exactly as periods of atoms are above.
#
# Inside tokens an index is named by its depth -- _v0 for a block at the top,
# _v1 inside it -- so equal blocks are equal tokens wherever they sit. Letters
# are given only when the tree is written out.

ONE_KEY = ()
NAMES = "kjimnpqrst"         # written index names, outermost first
TOKEN_PERIOD = 32            # tokens in one period
TWO_PERIOD_MOVING_BODY = 1 / 2


@dataclass(frozen=True)
class TRow:
    entries: tuple          # one entry per number: ((key, c), ...), key () first


@dataclass(frozen=True)
class TRep:
    var: str | None         # None: a plain repeat count
    start: tuple            # entries, in the indices of enclosing blocks
    stop: tuple
    body: tuple             # tokens


def _const(c) -> tuple:
    return ((ONE_KEY, F(c)),)


E1 = _const(1)


def _norm(terms: dict) -> tuple:
    """An entry from {key: c}: key () always first, zero terms dropped."""
    one = terms.pop(ONE_KEY, F(0))
    rest = sorted(((k, c) for k, c in terms.items() if c != 0), key=repr)
    return ((ONE_KEY, one),) + tuple(rest)


def _kmul(a: tuple, b: tuple) -> tuple:
    """The product of two keys, like factors merged."""
    pw: dict = {}
    ex: dict = {}
    for f in a + b:
        if f[0] == "p":
            pw[f[1]] = pw.get(f[1], 0) + f[2]
        else:
            ex[f[2]] = ex.get(f[2], 1) * f[1]
    return tuple(sorted([("p", v, e) for v, e in pw.items()]
                        + [("x", b_, v) for v, b_ in ex.items()], key=repr))


def _entry(v, scope: dict) -> tuple | None:
    """A number, or a sympy expression in the indices of `scope` (written
    name -> depth name), as an entry; None for a form tokens cannot hold."""
    if isinstance(v, (int, F)):
        return _const(v)
    if isinstance(v, sympy.Basic) and v.is_Rational:
        return _const(F(int(v.p), int(v.q)))
    if not isinstance(v, sympy.Basic):
        return None
    terms: dict = {}
    for term in sympy.Add.make_args(sympy.expand(v)):
        c, key = F(1), ()
        for f in sympy.Mul.make_args(term):
            if f.is_Rational:
                c *= F(int(f.p), int(f.q))
            elif f.is_Symbol and f.name in scope:
                key = _kmul(key, (("p", scope[f.name], 1),))
            elif f.is_Pow and f.base.is_Symbol and f.base.name in scope \
                    and f.exp.is_Integer and f.exp > 0:
                key = _kmul(key, (("p", scope[f.base.name], int(f.exp)),))
            elif f.is_Pow and f.base.is_Integer and f.base > 1:
                b = int(f.base)
                for t in sympy.Add.make_args(sympy.expand(f.exp)):
                    if t.is_Integer:
                        c *= F(b) ** int(t)
                        continue
                    cc, s = t.as_coeff_Mul()
                    if not (s.is_Symbol and s.name in scope and cc.is_Integer and cc > 0):
                        return None
                    key = _kmul(key, (("x", b ** int(cc), scope[s.name]),))
            else:
                return None
        terms[key] = terms.get(key, F(0)) + c
    return _norm(terms)


def _ev(entry: tuple, env: dict):
    """The value of an entry with the indices bound (an int or a Fraction)."""
    total = 0
    for key, c in entry:
        v = 1
        for f in key:
            x = env[f[1] if f[0] == "p" else f[2]]
            if f[0] == "p":
                v *= x ** f[2]
            else:
                v *= f[1] ** x if x >= 0 else F(1, f[1] ** -x)
        total += c * v
    return total


def _xp(tok, env: dict, out: list, budget: int) -> bool:
    """Append the rows a token expands to; False if it is not valid there (a
    number that is not whole) or would pass `budget` rows."""
    if isinstance(tok, TRow):
        vals = [_ev(e, env) for e in tok.entries]
        if any(getattr(v, "denominator", 1) != 1 for v in vals):
            return False
        out.append(tuple(int(v) for v in vals))
        return len(out) <= budget
    a, b = _ev(tok.start, env), _ev(tok.stop, env)
    if getattr(a, "denominator", 1) != 1 or getattr(b, "denominator", 1) != 1:
        return False
    if b - a + 1 > budget:
        return False
    for i in range(int(a), int(b) + 1):
        inner = env if tok.var is None else {**env, tok.var: i}
        for t in tok.body:
            if not _xp(t, inner, out, budget):
                return False
    return True


def _tl(tok) -> int:
    """Leaves of a token, as `leaves` counts them."""
    return 1 if isinstance(tok, TRow) else sum(_tl(t) for t in tok.body)


_SIG: dict = {}


def _sig(tok):
    """(shape, parameters, which parameters are row entries)."""
    got = _SIG.get(tok)
    if got is None:
        params: list = []
        mask: list = []

        def ent(e, in_row):
            params.extend(c for _, c in e)
            mask.extend([in_row] * len(e))
            return tuple(k for k, _ in e)

        def walk(t):
            if isinstance(t, TRow):
                return ("r",) + tuple(ent(e, True) for e in t.entries)
            return ("c", t.var, ent(t.start, False), ent(t.stop, False),
                    tuple(walk(b) for b in t.body))

        got = (walk(tok), tuple(params), tuple(mask))
        if len(_SIG) > 200_000:
            _SIG.clear()
        _SIG[tok] = got
    return got


def _basis_key(b, var: str) -> tuple:
    return {"1": (), "k": (("p", var, 1),), "k2": (("p", var, 2),)}.get(
        b, (("x", b, var),) if not isinstance(b, str) else None)


def _unsig(shape, vals, var: str | None = None):
    """A token of this shape with these parameters. Given `var`, the values
    are Formulas in that index and the token is the symbolic template."""
    def ent(keys):
        if var is None:
            return tuple((k, next(vals)) for k in keys)
        terms: dict = {}
        for k in keys:
            for b, c in next(vals).terms:
                nk = _kmul(k, _basis_key(b, var))
                terms[nk] = terms.get(nk, F(0)) + c
        return _norm(terms)

    def walk(s):
        if s[0] == "r":
            return TRow(tuple(ent(keys) for keys in s[1:]))
        _, v, ks, ke, body = s
        start, stop = ent(ks), ent(ke)
        return TRep(v, start, stop, tuple(walk(b) for b in body))

    return walk(shape)


def _mapvars(tok, f):
    """The token with every index name passed through f."""
    def key(k):
        return tuple(sorted(((x[0], f(x[1]), x[2]) if x[0] == "p" else (x[0], x[1], f(x[2]))
                            for x in k), key=repr))

    def ent(e):
        return _norm({key(k): c for k, c in e})

    if isinstance(tok, TRow):
        return TRow(tuple(ent(e) for e in tok.entries))
    return TRep(f(tok.var) if tok.var else None, ent(tok.start), ent(tok.stop),
                tuple(_mapvars(t, f) for t in tok.body))


def _shift(tok, d0: int):
    """Move a token one level deeper: index _vd becomes _v(d+1) for d >= d0."""
    def f(name):
        if name.startswith("_v") and int(name[2:]) >= d0:
            return f"_v{int(name[2:]) + 1}"
        return name
    return _mapvars(tok, f)


def _uses(tok, var: str) -> bool:
    def ent(e):
        return any(var in (x[1], x[2]) for k, _ in e for x in k)
    if isinstance(tok, TRow):
        return any(ent(e) for e in tok.entries)
    return ent(tok.start) or ent(tok.stop) or any(_uses(t, var) for t in tok.body)


def _from_tree(item, depth: int = 0, scope: dict | None = None):
    """A tree item (row or IC) as a token, or None if it holds a form tokens
    cannot (a row inside a row, an index in an exponent's base...)."""
    scope = scope or {}
    if isinstance(item, IC):
        it = item.it
        inner = dict(scope)
        if it.is_count:
            var, start = None, E1
        else:
            var = f"_v{depth}"
            inner[it.var] = var
            start = _entry(it.start, scope)
        stop = _entry(it.stop, scope)
        body = [_from_tree(a, depth + 1, inner) for a in item.args]
        if start is None or stop is None or any(b is None for b in body):
            return None
        return TRep(var, start, stop, tuple(body))
    if isinstance(item, (list, tuple)) and not isinstance(item, Seq):
        entries = [_entry(x, scope) for x in item]
        return None if any(e is None for e in entries) else TRow(tuple(entries))
    return None


def _expr(entry: tuple, names: dict):
    out = sympy.Integer(0)
    for key, c in entry:
        term = sympy.Rational(c.numerator, c.denominator)
        for f in key:
            if f[0] == "p":
                term *= sympy.Symbol(names[f[1]]) ** f[2]
            else:
                term *= sympy.Integer(f[1]) ** sympy.Symbol(names[f[2]])
        out += term
    return int(out) if out.is_Integer else _shortest(out)


def _shortest(e):
    """The expression written the shortest way: as it is, factored, or with
    the terms in each set of indices factored apart -- k(k+1)/2 + j rather
    than k^2/2 + k/2 + j, as the paper writes Fig. 6(d)."""
    def size(x):
        return len(re.findall(r"\d+|[A-Za-z]\w*", str(x)))
    groups: dict = {}
    for t in sympy.Add.make_args(e):
        groups.setdefault(frozenset(t.free_symbols), []).append(t)
    split = sympy.Add(*[sympy.factor(sympy.Add(*ts)) for ts in groups.values()])
    return min((e, sympy.factor(e), split), key=size)


def _to_tree(tok, names: dict | None = None, level: int = 0):
    """A token written out as a tree item, its indices named k, j, i, ...
    outermost first. A block whose index is not used is a plain repeat."""
    names = names or {}
    if isinstance(tok, TRow):
        return tuple(_expr(e, names) for e in tok.entries)
    if tok.var is None or not any(_uses(t, tok.var) for t in tok.body):
        n = _expr(tok.stop, names)
        if tok.var is not None:
            n = sympy.expand(n - _expr(tok.start, names) + 1)
            n = int(n) if n.is_Integer else n
        return IC([_to_tree(t, names, level) for t in tok.body], Iter(None, 1, n))
    name = NAMES[level] if level < len(NAMES) else f"k{level}"
    inner = {**names, tok.var: name}
    return IC([_to_tree(t, inner, level + 1) for t in tok.body],
              Iter(name, _expr(tok.start, names), _expr(tok.stop, names), table=True))


def _atom_tokens(rows: list[Row], prog_min: int | None) -> list:
    """Rows as tokens: runs, and progressions when prog_min is given."""
    out: list = []
    for at in segment(rows, prog_min):
        row = TRow(tuple(_const(x) for x in at.a))
        if at.n == 1:
            out.append(row)
        elif not any(at.d):
            out.append(TRep(None, E1, _const(at.n), (row,)))
        else:
            ent = tuple(_norm({(): F(a - d), (("p", "_v0", 1),): F(d)})
                        for a, d in zip(at.a, at.d))
            out.append(TRep("_v0", E1, _const(at.n), (TRow(ent),)))
    return out


def _tandem(toks: list, depth: int = 0) -> list:
    """Exactly repeated groups of tokens as plain repeats, the group saving
    the most first, then the rest on either side the same way."""
    m = len(toks)
    if m < 2:
        return toks
    ids: dict = {}
    seq = [ids.setdefault(t, len(ids)) for t in toks]
    lv = [_tl(t) for t in toks]
    best = None
    for p in range(1, min(TOKEN_PERIOD, m // 2) + 1):
        _tick()
        for s in range(m - 2 * p + 1):
            if seq[s] != seq[s + p] or seq[s:s + p] != seq[s + p:s + 2 * p]:
                continue
            c = 2
            while s + (c + 1) * p <= m and seq[s + c * p:s + (c + 1) * p] == seq[s:s + p]:
                c += 1
            sav = (c - 1) * sum(lv[s:s + p])
            if best is None or sav > best[0]:
                best = (sav, s, p, c)
    if best is None:
        return toks
    _, s, p, c = best
    group = _tandem([_shift(t, depth) for t in toks[s:s + p]], depth + 1)
    return (_tandem(toks[:s], depth) + [TRep(None, E1, _const(c), tuple(group))]
            + _tandem(toks[s + c * p:], depth))


# --------------------------------------------------------------------------
# periodic blocks over tokens
# --------------------------------------------------------------------------

@dataclass
class TBlock:
    i0: int                 # tokens covered (as found, before extension)
    i1: int
    r0: int                 # rows covered, after extension (rows mode)
    r1: int
    tok: TRep
    saving: int
    cost: int

    @property
    def rank(self):
        return (self.saving, -self.cost)


def _tgrow(sigs, s: int, p: int) -> int:
    """Whole periods of p tokens from s with one shape per position whose
    parameters can still be fitted (loosely: _tblock confirms them)."""
    m = len(sigs)
    if s + 2 * p <= m and any(sigs[s + p + i][0] != sigs[s + i][0] for i in range(p)):
        return 1
    cols = [[Column(True) for _ in sigs[s + i][1]] for i in range(p)]
    q = 0
    while s + (q + 1) * p <= m:
        base = s + q * p
        if any(sigs[base + i][0] != sigs[s + i][0] for i in range(p)):
            break
        if not all(c.add(q + 1, v) for i in range(p)
                   for c, v in zip(cols[i], sigs[base + i][1])):
            break
        q += 1
    return q


def _tfit(sigs, s: int, p: int, q: int):
    out = []
    for i in range(p):
        fs = []
        for j in range(len(sigs[s + i][1])):
            f = fit([(t + 1, sigs[s + t * p + i][1][j]) for t in range(q)], loose=True)
            if f is None:
                return None
            fs.append(f)
        out.append(fs)
    return out


def _order(p: int, r: int, t: int):
    """(position, period) of virtual period t of a body rotated by r."""
    return [(i, t - 1) for i in range(p - r, p)] + [(i, t) for i in range(p - r)]


def _tperiod_rows(shapes, fs, r: int, t: int, budget: int):
    out: list = []
    for i, k in _order(len(shapes), r, t):
        tok = _unsig(shapes[i], iter([f(k) for f in fs[i]]))
        if not _xp(tok, {}, out, budget):
            return None
    return out


def _tperiod_toks(shapes, fs, t: int):
    return [_unsig(shapes[i], iter([f(t) for f in fs[i]])) for i in range(len(shapes))]


def _two_ok(fl, masks, share: float, plain_ok: bool) -> bool:
    """Two periods are a block only if every count stays put and few entries
    move (see TWO_PERIOD_MOVING). Over rows the plain steps of +-1 are always
    fine, as in _body; inside a body they count as moving too, or a body's
    {2,2,2},{1,3,3} would become {3-k,k+1,k+1}."""
    flat = [(f, m) for fs, ms in zip(fl, masks) for f, m in zip(fs, ms)]
    if plain_ok and all(f.plain_line for f, _ in flat):
        return True
    if any(not f.is_const for f, m in flat if not m):
        return False
    values = [f for f, m in flat if m]
    return sum(not f.is_const for f in values) <= share * len(values)


def _tblock(toks: list, depth: int, rows=None, offs=None) -> TBlock | None:
    """The periodic block over tokens saving the most. With rows, candidates
    are extended at the row level (any rotation) and saving is counted
    against runs of the rows; without, periods are matched as equal tokens."""
    m = len(toks)
    if m < 4:
        return None
    sigs = [_sig(t) for t in toks]
    lv = [_tl(t) for t in toks]
    found = []
    for p in range(1, min(TOKEN_PERIOD, m // 2) + 1):
        for phase in range(p):
            s = phase
            while s + 2 * p <= m:
                _tick()
                q = _tgrow(sigs, s, p)
                if q >= 2:
                    found.append((sum(lv[s:s + q * p]) - sum(lv[s:s + p]), s, p, q))
                s += max(1, q - 1) * p
    found.sort(key=lambda c: (-c[0], c[2]))
    best: TBlock | None = None
    for _, s, p, q in found[:EXTEND_TOP]:
        fs = None
        while q >= 2 and (fs := _tfit(sigs, s, p, q)) is None:
            q -= 1
        if fs is None:
            continue
        shapes = [sigs[s + i][0] for i in range(p)]
        masks = [sigs[s + i][2] for i in range(p)]
        for r in (range(p) if rows is not None else (0,)):
            _tick()
            if rows is not None:
                if r == 0:
                    lo, hi, r0, r1 = 1, q, offs[s], offs[s + q * p]
                else:
                    lo, hi, r0, r1 = 2, q, offs[s + p - r], offs[s + q * p - r]
                for _ in range(len(rows)):
                    got = _tperiod_rows(shapes, fs, r, lo - 1, r0)
                    if not got or rows[r0 - len(got):r0] != got:
                        break
                    lo, r0 = lo - 1, r0 - len(got)
                for _ in range(len(rows)):
                    got = _tperiod_rows(shapes, fs, r, hi + 1, len(rows) - r1)
                    if not got or rows[r1:r1 + len(got)] != got:
                        break
                    hi, r1 = hi + 1, r1 + len(got)
                i0, i1 = s, s + q * p
            else:
                lo, hi, i0, i1 = 1, q, s, s + q * p
                while i0 - p >= 0 and _tperiod_toks(shapes, fs, lo - 1) == toks[i0 - p:i0]:
                    lo, i0 = lo - 1, i0 - p
                while i1 + p <= m and _tperiod_toks(shapes, fs, hi + 1) == toks[i1:i1 + p]:
                    hi, i1 = hi + 1, i1 + p
                r0 = r1 = 0
            Q = hi - lo + 1
            if Q < 2:
                continue
            # refit so the index runs 1..Q, each formula confirmed on its points
            order = _order(p, r, 0)
            fl, ml = [], []
            for i, shift in order:
                row = [fit([(k, f(k + lo - 1 + shift)) for k in range(1, Q + 1)]) for f in fs[i]]
                if any(g is None for g in row):
                    break
                fl.append(row)
                ml.append(masks[i])
            else:
                # rows, and the top level matched as tokens, keep the rule of
                # _body; inside a body a little more may move (Fig. 1's
                # {2,2},{2,4} is €(j,1,2)[{2,2j}] in the paper)
                if Q == 2 and not (_two_ok(fl, ml, TWO_PERIOD_MOVING, True)
                                   if rows is not None or depth == 0
                                   else _two_ok(fl, ml, TWO_PERIOD_MOVING_BODY, False)):
                    continue
                body = tuple(_unsig(shapes[i], iter(row), "_new")
                             for (i, _), row in zip(order, fl))
                body = tuple(_mapvars(_shift(b, depth),
                                      lambda v: f"_v{depth}" if v == "_new" else v)
                             for b in body)
                var = f"_v{depth}" if any(_uses(b, f"_v{depth}") for b in body) else None
                tok = TRep(var, E1, _const(Q), body)
                if rows is not None:
                    saving = _rle_leaves(rows[r0:r1]) - _tl(tok)
                else:
                    saving = sum(lv[i0:i1]) - _tl(tok)
                cost = sum(f.cost for row in fl for f in row)
                cand = TBlock(i0, i1, r0, r1, tok, saving, cost)
                if saving > 0 and (best is None or cand.rank > best.rank):
                    best = cand
    return best


def _tcount(tok) -> int:
    out: list = []
    _xp(tok, {}, out, MAX_ROWS)
    return len(out)


def _lift(rows: list[Row], toks: list) -> list:
    """Blocks over the tokens of `rows`, the best first, then the rows on
    either side the same way (re-cut into tokens where a block's extension
    took part of one)."""
    if len(toks) < 4:
        return toks
    offs = [0]
    for t in toks:
        offs.append(offs[-1] + _tcount(t))
    b = _tblock(toks, 0, rows, offs)
    if b is None:
        return toks
    li = max(i for i in range(len(toks) + 1) if offs[i] <= b.r0)
    ri = min(i for i in range(len(toks) + 1) if offs[i] >= b.r1)
    left = toks[:li] + _atom_tokens(rows[offs[li]:b.r0], None)
    right = _atom_tokens(rows[b.r1:offs[ri]], None) + toks[ri:]
    return (_lift(rows[:b.r0], left) + [b.tok]
            + _lift(rows[b.r1:], right))


def _squeeze(toks: list, depth: int = 0, top: bool = True) -> list:
    """Blocks inside the bodies of blocks, at every depth: periods of the
    body's tokens, matched as tokens (the body's numbers may be formulas),
    and -- below the top -- pairs of stretches that differ by optional pieces
    (_gapped). The body of a repeat of one (a piece of _cut_tokens, spliced
    out when written) counts as the top."""
    toks = [_squeeze_one(t, depth, top) for t in toks]
    while True:
        found = [b for b in (_tblock(toks, depth),
                             None if top else _gapped(toks, depth)) if b is not None]
        if not found:
            return toks
        b = max(found, key=lambda x: x.rank)
        toks = toks[:b.i0] + [_squeeze_one(b.tok, depth, top)] + toks[b.i1:]


# Most pieces an optional run may hold, and at least how many pieces the two
# stretches must share around it.
GAP_MAX = 4
GAP_CORE = 3
GAP_SPAN = 16           # most pieces the two stretches share


def _gapped(toks: list, depth: int) -> TBlock | None:
    """Two neighbouring stretches that line up position by position once a
    run of extra pieces in one of them is left out, written as one body over
    an index with two values, the extra pieces under a count of 0 and 1 --
    Fig. 4(b) of the paper, made by hand there:

        €(m,0,1)[{1,2,3}, {2m+3n+1,2m+3n+2}, ..., €(m)[{1,2}, {1,3n+6}], {3n+2,3n+3}]

    The pieces both stretches share are fitted as for any two periods (the
    same _two_ok rule); the extra pieces are written once, as they are."""
    m = len(toks)
    # _squeeze calls this only inside a body, as in the paper: both stretches
    # then sit in every period of the block around them. At the top level two
    # stretches sharing a few rows and nothing else were paired around whole
    # blocks, and the search was slow on long rows.
    if m < 2 * GAP_CORE + 1:
        return None
    sigs = [_sig(t) for t in toks]
    lv = [_tl(t) for t in toks]
    best: TBlock | None = None
    for s in range(m):
        # Such a block saves exactly the leaves of the stretch it no longer
        # writes twice, wherever the gap is: longest first, and nothing is
        # fitted that cannot beat the best so far.
        for a in range(min(GAP_SPAN, (m - s) // 2), GAP_CORE - 1, -1):
            for g in range(1, GAP_MAX + 1):
                _tick()
                for first_long in (False, True):
                    la, lb = (a + g, a) if first_long else (a, a + g)
                    if s + la + lb > m:
                        continue
                    short = list(range(s + la, s + la + lb)) if first_long else list(range(s, s + la))
                    if best is not None and sum(lv[x] for x in short) <= best.saving:
                        continue
                    long_ = list(range(s, s + la)) if first_long else list(range(s + la, s + la + lb))
                    # the gap sits where the shapes stop matching from the
                    # front and start matching from the back
                    front = 0
                    while front < a and sigs[short[front]][0] == sigs[long_[front]][0]:
                        front += 1
                    back = 0
                    while back < a and sigs[short[-1 - back]][0] == sigs[long_[-1 - back]][0]:
                        back += 1
                    for j in range(a - back, front + 1):
                        core = long_[:j] + long_[j + g:]
                        cand = _gapped_block(toks, sigs, depth, s, la + lb, short, core,
                                             long_[j:j + g], j, first_long)
                        if cand is None:
                            continue
                        cand.saving = sum(lv[s:s + la + lb]) - _tl(cand.tok)
                        if cand.saving > 0 and (best is None or cand.rank > best.rank):
                            best = cand
    return best


def _gapped_block(toks, sigs, depth, s, n, short, core, extra, j, first_long):
    # period 1 is the first stretch, period 2 the second
    p1, p2 = (core, short) if first_long else (short, core)
    fl, ml, shapes = [], [], []
    for x, y in zip(p1, p2):
        row = [fit([(1, u), (2, v)]) for u, v in zip(sigs[x][1], sigs[y][1])]
        if any(f is None for f in row):
            return None
        fl.append(row)
        ml.append(sigs[x][2])
        shapes.append(sigs[x][0])
    if not (_two_ok(fl, ml, TWO_PERIOD_MOVING, True) if depth == 0
            else _two_ok(fl, ml, TWO_PERIOD_MOVING_BODY, False)):
        return None
    body = [_unsig(sh, iter(row), "_new") for sh, row in zip(shapes, fl)]
    # the extra pieces: once in the stretch that has them, not in the other
    count = {(): F(2), (("p", "_new", 1),): F(-1)} if first_long \
        else {(): F(-1), (("p", "_new", 1),): F(1)}
    body.insert(j, TRep(None, E1, _norm(count), tuple(_shift(toks[e], depth) for e in extra)))
    body = tuple(_mapvars(_shift(b, depth), lambda v: f"_v{depth}" if v == "_new" else v)
                 for b in body)
    tok = TRep(f"_v{depth}", E1, _const(2), body)
    return TBlock(s, s + n, 0, 0, tok, 0, sum(f.cost for row in fl for f in row) + 2)


def _squeeze_one(tok, depth: int, top: bool = False):
    if isinstance(tok, TRow):
        return tok
    wrapper = top and tok.var is None and tok.stop == E1
    return TRep(tok.var, tok.start, tok.stop,
                tuple(_squeeze(list(tok.body), depth + 1, wrapper)))


def _token_forms(rows: list[Row], tree: list, deep: bool = True) -> list[list]:
    """More forms for the rows, from the token layer: periods over the items
    of `tree` (the search above), over exactly repeated groups of rows, and
    -- when `deep` -- over pieces cut where one kind of row recurs; then the
    bodies of every block searched again."""
    starts = []
    first = [_from_tree(x) for x in tree]
    if all(t is not None for t in first):
        starts.append(first)
    starts.append(_tandem(_atom_tokens(rows, 3)))
    starts.append(_tandem(_atom_tokens(rows, None)))
    if deep:
        starts.extend(_cut_tokens(rows))
    out = []
    for toks in starts:
        for _ in range(4):
            new = _lift(rows, toks)
            if sum(map(_tl, new)) >= sum(map(_tl, toks)):
                break
            toks = new
        toks = _squeeze(toks)
        out.append(_unwrap([_to_tree(t) for t in toks]))
    return out


# Kinds of row tried as the start of an outer period, rarest first.
CUT_KINDS = 3


def _kind(row: Row) -> tuple:
    """A row with every number but 0 and 1 blanked, as ReduceSetList's
    generic pass compares them."""
    return (len(row),) + tuple(x if x in (0, 1) else None for x in row)


def _cut_tokens(rows: list[Row]) -> list[list]:
    """Top-down starts: the rows cut before each row of one kind, each piece
    reduced on its own and made one token. When period n of the outer block
    holds n inner blocks (€n[{n}, €(i,1,n)[...]]) no period of the pieces
    below lines up; the pieces themselves do, and _lift fits across them."""
    count: dict = {}
    for r in rows:
        count[_kind(r)] = count.get(_kind(r), 0) + 1
    kinds = sorted((c, repr(g), g) for g, c in count.items() if 2 <= c <= len(rows) // 3)
    out = []
    for _, _, g in kinds[:CUT_KINDS]:
        cuts = [i for i, r in enumerate(rows) if _kind(r) == g]
        toks = _atom_tokens(rows[:cuts[0]], None)
        for a, b in zip(cuts, cuts[1:] + [len(rows)]):
            piece = _best_tokens(rows[a:b])
            # one token for the piece: a repeat of one, unwrapped when written
            toks.append(TRep(None, E1, E1, tuple(_shift(t, 0) for t in piece)))
        out.append(toks)
    return out


def _best_tokens(rows: list[Row]) -> list:
    """The smallest form of a piece of the rows, as tokens. The pieces' own
    search is not reported to the caller, only given its chances to stop."""
    cb = getattr(_trace, "cb", None)
    _trace.cb = None if cb is None else (lambda ev: cb(None))
    try:
        best = _rle_tree(rows)
        if len(rows) >= 2:
            tree = _reduce(rows)
            for form in [tree] + _token_forms(rows, tree, deep=False):
                if (leaves(form), written(form)) < (leaves(best), written(best)) \
                        and [tuple(x) for x in expand(form)] == rows:
                    best = form
    finally:
        _trace.cb = cb
    toks = [_from_tree(x) for x in best]
    return toks if all(t is not None for t in toks) else _atom_tokens(rows, None)


def _unwrap(tree):
    """Repeats of one spliced into what holds them."""
    def items(xs):
        out = []
        for x in xs:
            x = _unwrap(x)
            if isinstance(x, IC) and x.it.is_count and x.it.stop == 1:
                out.extend(x.args)
            else:
                out.append(x)
        return out
    if isinstance(tree, IC):
        return IC(items(tree.args), tree.it)
    if isinstance(tree, list):
        return items(tree)
    return tree


def _leaf_exprs(item):
    if isinstance(item, IC):
        for a in item.args:
            yield from _leaf_exprs(a)
        if not item.it.is_count:
            yield item.it.start
        yield item.it.stop
    elif isinstance(item, (list, tuple)):
        for a in item:
            yield from _leaf_exprs(a)
    elif isinstance(item, sympy.Basic):
        yield item


def _subs(item, v, e):
    def num(x):
        if not isinstance(x, sympy.Basic):
            return x
        x = sympy.expand(x.subs(v, e))
        return int(x) if x.is_Integer else _shortest(x)
    if isinstance(item, IC):
        it = item.it
        nit = Iter(it.var, num(it.start) if not it.is_count else it.start, num(it.stop),
                   it.upper, it.table)
        return IC([_subs(a, v, e) for a in item.args], nit)
    if isinstance(item, tuple):
        return tuple(_subs(a, v, e) for a in item)
    if isinstance(item, list):
        return [_subs(a, v, e) for a in item]
    return num(item)


# Highest number an index may be moved to start from; an index may always
# start at an expression in the indices around it (€(j,k,6)).
REINDEX_MAX = 10


def _reindex(tree):
    """Each index started where its body reads simplest: €(j,k,6)[{j}] rather
    than €(j,1,7-k)[{j+k-1}]. The rows are the same; only the writing moves."""
    if isinstance(tree, list):
        return [_reindex(x) for x in tree]
    if not isinstance(tree, IC):
        return tree
    ic = IC([_reindex(a) for a in tree.args], tree.it)
    it = ic.it
    if it.is_count:
        return ic
    v = sympy.Symbol(it.var)
    offsets = set()
    for e in _leaf_exprs(ic.args):
        c = sympy.expand(e).coeff(v)
        if c in (1, -1):
            rest = sympy.expand(e - c * v)
            if rest != 0 and v not in rest.free_symbols:
                offsets.add(rest if c == 1 else -rest)
    best, size = ic, written([ic])
    for o in sorted(offsets, key=str):
        cand = _subs(ic, v, v - o)
        start = sympy.expand(it.start + o)
        stop = sympy.expand(it.stop + o)
        if start.is_Integer and not 0 <= start <= REINDEX_MAX:
            continue                # €(k,-2,-1), €(k,71,72): shorter, not clearer
        cand = IC(cand.args, Iter(it.var, int(start) if start.is_Integer else _shortest(start),
                                  int(stop) if stop.is_Integer else _shortest(stop),
                                  it.upper, it.table))
        if written([cand]) < size:
            best, size = cand, written([cand])
    return best


def _row_runs(tree):
    """Runs of equal numbers inside a row written as one, {1,€(3)[3]} for
    {1,3,3,3} as the paper writes Fig. 5(h) -- wherever that is shorter."""
    if isinstance(tree, IC):
        return IC([_row_runs(a) for a in tree.args], tree.it)
    if isinstance(tree, list):
        return [_row_runs(x) for x in tree]
    if not isinstance(tree, tuple):
        return tree
    out, i = [], 0
    while i < len(tree):
        j = i
        while j + 1 < len(tree) and not isinstance(tree[i], IC) and tree[j + 1] == tree[i]:
            j += 1
        c = j - i + 1
        one = written([(tree[i],)])
        if c >= 2 and c * one > 1 + one:
            out.append(IC([tree[i]], Iter(None, 1, c)))
        else:
            out.extend(tree[i:j + 1])
        i = j + 1
    return tuple(out)


@dataclass
class Result:
    tree: list
    rows: list
    verified: bool

    @property
    def text(self) -> str:
        return render(self.tree).replace("**", "^")

    @property
    def size(self) -> int:
        return leaves(self.tree)


def concatenate(rows, on_event=None) -> Result:
    """Reduce rows to nested indexed concatenations, losslessly.

    With `on_event`, each step of the search is reported as it happens: a
    "round" for each stretch of rows searched, a "try" for each candidate
    block (status best, beaten, nofit, short or refit), then "keep" or "none".
    Rows are numbered from 0, r1 exclusive. Between events it is also called
    with None, often, so that raising Stopped abandons the search promptly."""
    rows = [tuple(int(x) for x in r) for r in rows]
    if len(rows) > MAX_ROWS:
        return Result(_rle_tree(rows), rows, True)
    _trace.cb, _trace.base, _trace.round = on_event, 0, 0
    try:
        tree = _reduce(rows)
        ok = [tuple(x) for x in expand(tree)] == rows
        if not ok:                       # never hand back something wrong
            tree = _rle_tree(rows)
        # The token layer can only add forms: one is taken only if it is
        # smaller and expands back to the rows exactly.
        size = (leaves(tree), written(tree))
        for form in [tree] + _token_forms(rows, tree):
            form = _row_runs(_reindex(form))
            if (leaves(form), written(form)) < size \
                    and [tuple(x) for x in expand(form)] == rows:
                tree, size, ok = form, (leaves(form), written(form)), True
    finally:
        _trace.cb = None
    return Result(tree, rows, ok)
