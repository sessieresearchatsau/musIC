"""Fit a closed form to an integer sequence.

Mathematica's FindSequenceFunction has no Python equivalent, and ReduceSetList
leans on it heavily: FindSeqFns transposes the repeated blocks into columns and
asks for a formula per column. This module supplies that.

The contract is deliberately conservative. Every candidate is fitted on the
fewest points that determine it and then *checked against the rest*, so a fit is
only returned when it predicts data it never saw. A degree-2 polynomial through
three points proves nothing; the same polynomial predicting points four and five
does. When nothing survives, we return None and the caller keeps the literal
values -- which is why the reduction stays lossless.
"""
from __future__ import annotations

from fractions import Fraction

import sympy

Num = int | Fraction


def _exact(v) -> Fraction:
    return v if isinstance(v, Fraction) else Fraction(v)


def _as_expr(value: Fraction) -> sympy.Expr:
    return sympy.Rational(value.numerator, value.denominator)


def fit(values: list[Num], var: str = "n", start: int = 1, min_extra: int = 1):
    """Find f such that f(start + k) == values[k], or None.

    Tried in order of simplicity: constant, polynomial, geometric,
    polynomial times geometric, then an order-2 linear recurrence.

    `min_extra` is how many points beyond the ones that determine a candidate
    must also check out. The default of 1 refuses a fit that merely interpolates
    everything it was given. Mathematica is more permissive -- the notebooks rely
    on FindSequenceFunction[{72,73}] returning 71+#1& -- so the reducer passes 0
    and labels those fits as extrapolations.
    """
    if not values:
        return None
    vals = [_exact(v) for v in values]
    n = sympy.Symbol(var)

    for fitter in (_constant, _polynomial, _geometric, _poly_geometric, _crecur):
        expr = fitter(vals, n, start, min_extra)
        if expr is not None and _verifies(expr, n, vals, start):
            return sympy.simplify(expr)
    return None


def is_extrapolation(values: list[Num]) -> bool:
    """True when there is no spare point to test a linear fit against, so the
    formula is a guess beyond the evidence."""
    return len(values) < 3 and len(set(values)) > 1


def _verifies(expr, n, vals, start) -> bool:
    for k, want in enumerate(vals):
        got = expr.subs(n, start + k)
        if not got.is_number:
            return False
        if sympy.nsimplify(got) != _as_expr(want):
            return False
    return True


# --------------------------------------------------------------------------

def _constant(vals, n, start, min_extra=1):
    return _as_expr(vals[0]) if len(set(vals)) == 1 else None


def _polynomial(vals, n, start, min_extra=1):
    """Lowest-degree polynomial that also predicts the points it was not fitted on.

    Degree d needs d+1 points to determine and at least one more to test, so a
    sequence of length L can support degree up to L-2. This is the guard against
    the trivial interpolant through every point.
    """
    L = len(vals)
    # Degree d needs d+1 points to determine and one more to test. Relaxing
    # that is only defensible for a straight line through two points, which is
    # what Mathematica does and the notebooks depend on; a cubic through
    # exactly four points asserts nothing at all, so it stays barred.
    degrees = list(range(1, min(L - 2, 4) + 1))
    if min_extra == 0 and L >= 2 and 1 not in degrees:
        degrees.insert(0, 1)
    for deg in degrees:
        pts = [(sympy.Integer(start + k), _as_expr(vals[k])) for k in range(deg + 1)]
        try:
            expr = sympy.expand(sympy.interpolate(pts, n))
        except Exception:
            continue
        # Check here, not only in fit(): a low degree that interpolates the
        # first points but fails later must not stop us trying a higher one.
        if _verifies(expr, n, vals, start):
            return expr
    return None


def _geometric(vals, n, start, min_extra=1):
    """a * r**(n-start). Needs 3 points: two fix it, the third tests it."""
    if len(vals) < 2 + min_extra or vals[0] == 0:
        return None
    if any(v == 0 for v in vals[:-1]):
        return None
    r = vals[1] / vals[0]
    if r == 1:
        return None                      # that is the constant case
    return _as_expr(vals[0]) * sympy.Rational(r.numerator, r.denominator) ** (n - start)


def _poly_geometric(vals, n, start, min_extra=1):
    """(linear in n) * r**(n-start) -- e.g. durations halving while pitch climbs."""
    if len(vals) < 4 or any(v == 0 for v in vals):
        return None
    ratios = [vals[k + 1] / vals[k] for k in range(len(vals) - 1)]
    # For a(k) = P(k)*r**k the ratios tend to r, so the later ones are the best
    # guesses; a few small values cover the rest.
    cands = {ratios[-1], ratios[0], Fraction(2), Fraction(3), Fraction(4),
             Fraction(1, 2), Fraction(1, 3)}
    for r in cands:
        if r in (0, 1):
            continue
        divided = [vals[k] / (r ** k) for k in range(len(vals))]
        sub = _polynomial(divided, n, start, min_extra)
        if sub is not None and _verifies(sub, n, divided, start):
            return sub * sympy.Rational(r.numerator, r.denominator) ** (n - start)
    return None


def _crecur(vals, n, start, min_extra=1):
    """Order-2 linear recurrence a(k) = p a(k-1) + q a(k-2), solved in closed form.

    Needs 5 terms: two seeds, two to solve for p and q, one to check.
    """
    if len(vals) < 5:
        return None
    p, q = sympy.symbols("p q")
    eqs = [sympy.Eq(_as_expr(vals[k]), p * _as_expr(vals[k - 1]) + q * _as_expr(vals[k - 2]))
           for k in (2, 3)]
    try:
        sol = sympy.solve(eqs, [p, q], dict=True)
    except Exception:
        return None
    if not sol:
        return None
    pv, qv = sol[0].get(p), sol[0].get(q)
    if pv is None or qv is None or not (pv.is_number and qv.is_number):
        return None
    for k in range(4, len(vals)):
        if _as_expr(vals[k]) != pv * _as_expr(vals[k - 1]) + qv * _as_expr(vals[k - 2]):
            return None
    # Closed form from the characteristic roots; rejected later if not exact.
    x = sympy.Symbol("x")
    roots = sympy.roots(x**2 - pv * x - qv, x)
    if len(roots) != 2 or any(m != 1 for m in roots.values()):
        return None
    (r1, _), (r2, _) = roots.items()
    if any(not r.is_rational for r in (r1, r2)):
        return None            # a Binet-style form is exact but unreadable
    c1, c2 = sympy.symbols("c1 c2")
    sol2 = sympy.solve(
        [sympy.Eq(c1 * r1**0 + c2 * r2**0, _as_expr(vals[0])),
         sympy.Eq(c1 * r1**1 + c2 * r2**1, _as_expr(vals[1]))], [c1, c2], dict=True)
    if not sol2:
        return None
    k = n - start
    return sympy.simplify(sol2[0][c1] * r1**k + sol2[0][c2] * r2**k)
