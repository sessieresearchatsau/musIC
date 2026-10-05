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

Nothing is returned unless it expands back to the input exactly.
"""
from __future__ import annotations

from dataclasses import dataclass
from fractions import Fraction as F

import sympy

from .ic import IC, Iter, expand, render

Row = tuple

K = sympy.Symbol("k")
J = sympy.Symbol("j")

MAX_PERIOD = 64        # atoms in one period
MAX_ROWS = 20000
EXTEND_TOP = 24        # candidates taken to the row-level extension step


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
    yield ("1",), 0
    yield ("1", "k"), 0
    for b in BASES:
        yield ("1", b), x
    yield ("1", "k", "k2"), x
    for b in BASES:
        yield ("1", "k", b), x


@dataclass(frozen=True)
class Formula:
    terms: tuple            # ((basis, coefficient), ...)

    def __call__(self, k: int) -> F:
        return sum((c * _basis_value(b, k) for b, c in self.terms), F(0))

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
        v = F(v)
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


def _body(atoms: list[Atom], s: int, p: int, q: int) -> Body | None:
    """The formulas for q periods from atom s, each confirmed on all its points."""
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
    if any(len(c.pts) == 2 and not c.f.plain_line for c in every):
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
    for prog_min in (3, None):
        atoms = segment(rows, prog_min)
        off = [0]
        for at in atoms:
            off.append(off[-1] + at.n)
        for _, s, p, q in _candidates(atoms)[:EXTEND_TOP]:
            body = None
            while q >= 2 and (body := _body(atoms, s, p, q)) is None:
                q -= 1          # the last period was only loosely fitted
            if body is None:
                continue
            for r in range(p):
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
                if best is not None and saving < best.saving:
                    continue
                rb = _rebase(body, r, lo, hi)
                if rb is None:
                    continue
                cand = Block(r0, r1, rb, hi - lo + 1, saving)
                if best is None or cand.rank > best.rank:
                    best = cand
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
        items.append(IC([row], Iter("j", 1, _num(n))))
    return IC(items, Iter("k", 1, block.q))


def _reduce(rows: list[Row]) -> list:
    if len(rows) < 2:
        return list(rows)
    block = _best_block(rows)
    if block is None:
        return _rle_tree(rows)
    return (_reduce(rows[:block.r0]) + [_block_tree(block)]
            + _reduce(rows[block.r1:]))


def leaves(tree) -> int:
    """Rows written down to state the expression: a row is 1, an IC is the
    leaves of its body."""
    return sum(leaves(x.args) if isinstance(x, IC) else 1 for x in tree)


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


def concatenate(rows) -> Result:
    """Reduce rows to nested indexed concatenations, losslessly."""
    rows = [tuple(int(x) for x in r) for r in rows]
    if len(rows) > MAX_ROWS:
        return Result(_rle_tree(rows), rows, True)
    tree = _reduce(rows)
    ok = [tuple(x) for x in expand(tree)] == rows
    if not ok:                       # never hand back something wrong
        tree = _rle_tree(rows)
    return Result(tree, rows, ok)
