"""Indexed Concatenation Notation: expansion, verification, reduction.

Paper: Caviness, Davis, Renck, Sarr, Anderson, Robles & Sharpe, "Indexed
Concatenation Notation: A Novel Way to Summarize Networks and Other Complex
Systems", SIMULTECH 2025, 39-50.  DOI 10.5220/0013514700003970

Conventions follow the Mathematica notebooks in 2026/new experiments/, where
the iterator is written  EURO_(var |= start)^(stop)  -- start is often 0, and
an index-free EURO with only an overscript means plain repetition.

Node types (JSON-friendly):
    int / str        a value, or a formula in the live index vars ("74-2*i")
    {"list": [...]}  a genuine sublist  -- {} delimiters survive
    {"seq":  [...]}  a subsequence      -- [] delimiters vanish into the parent
    {"ic": {"var","start","stop","body"}}   an indexed concatenation
                     var may be null for the bare repetition form.

An IC always splices its body into its parent: concatenation of lists is a
list and never adds a nesting level (paper eqs. 17-19).
"""

import json
from fractions import Fraction as F
from math import gcd

MAX_DEGREE = 3


def _lcm(a, b):
    return a * b // gcd(a, b)


# --------------------------------------------------------------------------
# expansion
# --------------------------------------------------------------------------

_SAFE = {"__builtins__": {}, "F": F, "Fraction": F}


def _val(x, env):
    if isinstance(x, bool):
        raise TypeError("bool is not an ICN value")
    if isinstance(x, int):
        return x
    if isinstance(x, F):
        if x.denominator != 1:
            raise ValueError(f"non-integer value {x}")
        return int(x)
    v = F(eval(x, _SAFE, {k: F(n) for k, n in env.items()}))
    if v.denominator != 1:
        raise ValueError(f"formula {x!r} gave non-integer {v} at {env}")
    return int(v)


def expand(node, env=None):
    """Expand one node into the items it splices into its parent."""
    env = env or {}
    if isinstance(node, (int, str, F)):
        return [_val(node, env)]
    if "list" in node:
        return [[y for n in node["list"] for y in expand(n, env)]]
    if "seq" in node:
        return [y for n in node["seq"] for y in expand(n, env)]
    if "ic" in node:
        ic = node["ic"]
        start, stop = _val(ic["start"], env), _val(ic["stop"], env)
        out = []
        for k in range(start, stop + 1):
            e = dict(env) if ic.get("var") is None else dict(env, **{ic["var"]: k})
            for n in ic["body"]:
                out.extend(expand(n, e))
        return out
    raise ValueError(f"unknown node: {node!r}")


def expand_all(nodes, env=None):
    return [y for n in nodes for y in expand(n, env or {})]


def verify(nodes, target):
    """True iff the ICN form reproduces target exactly -- the lossless check."""
    try:
        return expand_all(nodes) == target
    except Exception:
        return False


# --------------------------------------------------------------------------
# closed-form fitting
# --------------------------------------------------------------------------

def _fact(n):
    r = 1
    for i in range(2, n + 1):
        r *= i
    return r


def _binom(n, k):
    return _fact(n) // (_fact(k) * _fact(n - k))


def _check(formula, var, start, seq):
    try:
        return [_val(formula, {var: start + i}) for i in range(len(seq))] == seq
    except Exception:
        return False


def _poly_str(coef, var):
    """Render exact Fraction coefficients (index d = x**d) as a formula."""
    den = 1
    for c in coef:
        den = _lcm(den, c.denominator)
    terms = []
    for d in range(len(coef) - 1, -1, -1):
        n = int(coef[d] * den)
        if n == 0:
            continue
        p = "" if d == 0 else (f"*{var}" if d == 1 else f"*{var}**{d}")
        body = p[1:] if (abs(n) == 1 and d > 0) else f"{abs(n)}{p}"
        terms.append(("-" if n < 0 else "+", body))
    if not terms:
        return "0"
    s = ("-" if terms[0][0] == "-" else "") + terms[0][1]
    for sign, body in terms[1:]:
        s += f" {sign} {body}"
    return f"({s})/{den}" if den != 1 else s


def fit_poly(seq, var, start, report=None):
    """Exact polynomial in var (running from `start`) through seq, or None."""
    n = len(seq)
    if n == 0:
        return None
    if len(set(seq)) == 1:
        return seq[0]
    rows = [[F(v) for v in seq]]
    while len(rows[-1]) > 1:
        prev = rows[-1]
        rows.append([prev[i + 1] - prev[i] for i in range(len(prev) - 1)])
    # degree = depth of the first constant difference row (a length-1 row is
    # trivially constant, which caps deg at n-1)
    deg = next(d for d, r in enumerate(rows) if len(set(r)) == 1)
    if deg < 1 or deg > MAX_DEGREE:
        return None

    # Newton forward form -> expanded coefficients in t = var - start
    coef = [F(0)] * (deg + 1)
    basis = [F(1)] + [F(0)] * deg          # running product of (t - j)
    for d in range(deg + 1):
        c = rows[d][0] / F(_fact(d))
        for j in range(d + 1):
            coef[j] += c * basis[j]
        nxt = [F(0)] * (deg + 2)
        for j in range(d + 1):
            nxt[j + 1] += basis[j]
            nxt[j] -= basis[j] * d
        basis = nxt[:deg + 1]

    if start:                              # shift t = var - start back to var
        shifted = [F(0)] * (deg + 1)
        for d in range(deg + 1):
            for j in range(d + 1):
                shifted[j] += coef[d] * _binom(d, j) * F((-start) ** (d - j))
        coef = shifted

    s = _poly_str(coef, var)
    if not _check(s, var, start, seq):
        return None
    if report is not None:
        report.append({"kind": "poly", "degree": deg, "points": n,
                       "confirming": n - (deg + 1)})
    return s


def fit_geom(seq, var, start, report=None):
    """Fit c*r**k with rational r -- the 32*2^i and 24/3^i duration patterns."""
    if len(seq) < 2 or any(v == 0 for v in seq) or len(set(seq)) == 1:
        return None
    r = F(seq[1], seq[0])
    if r == 1 or any(F(b, a) != r for a, b in zip(seq, seq[1:])):
        return None
    c = F(seq[0]) / r ** start
    p, q = r.numerator, r.denominator
    if q == 1:
        base = f"{p}**{var}"
    elif p == 1:
        base = f"F(1,{q})**{var}"
    else:
        base = f"F({p},{q})**{var}"
    if c == 1:
        s = base
    elif c.denominator == 1:
        s = f"{c.numerator}*{base}"
    else:
        s = f"F({c.numerator},{c.denominator})*{base}"
    if not _check(s, var, start, seq):
        return None
    if report is not None:
        report.append({"kind": "geom", "ratio": str(r), "points": len(seq),
                       "confirming": len(seq) - 2})
    return s


def fit(seq, var, start=0, report=None):
    """Best closed form for seq as a function of var from `start`, else None.

    Geometric is tried first: durations live on the 2^a*3^b lattice, where a
    geometric law is the musically meaningful one and a cubic through four
    points would be a coincidence rather than an explanation.

    A fit with 0 confirming points (degree d through exactly d+1 points, or
    c*r**k through 2 points) is pure interpolation: it is exact on the data
    given but predicts nothing.  Such fits are recorded in `report` with
    "confirming": 0 so callers can flag them instead of overclaiming.
    """
    if len(set(seq)) == 1:
        return seq[0]
    return (fit_geom(seq, var, start, report)
            or fit_poly(seq, var, start, report))


# --------------------------------------------------------------------------
# reduction
# --------------------------------------------------------------------------

def _lift(x):
    return {"list": [_lift(y) for y in x]} if isinstance(x, list) else x


def _shape(x):
    return ("L", tuple(_shape(y) for y in x)) if isinstance(x, list) else "n"


def _slots(x, path=()):
    if isinstance(x, list):
        return [p for i, y in enumerate(x) for p in _slots(y, path + (i,))]
    return [path]


def _get(x, path):
    for i in path:
        x = x[i]
    return x


def _put(x, path, v):
    if not path:
        return v
    y = list(x)
    y[path[0]] = _put(y[path[0]], path[1:], v)
    return y


def _cost(nodes):
    return len(json.dumps(nodes))


def _fit_blocks(blocks, var, start, report=None):
    """One IC body covering equal-shaped blocks, a closed form per slot."""
    first = blocks[0]
    if any(_shape(b) != _shape(first) for b in blocks):
        return None
    body = []
    for j, item in enumerate(first):
        cur = item
        for p in _slots(item):
            f = fit([_get(b[j], p) for b in blocks], var, start, report)
            if f is None:
                return None
            cur = _put(cur, p, f) if p else f
        body.append(_lift(cur) if isinstance(cur, list) else cur)
    return body


VARS = "nkjihgm"


def reduce_list(items, depth=0, index_origin=0, report=None):
    """Reduce a list to (nested) indexed concatenations. Always lossless."""
    if depth > 4 or len(items) < 2:
        return [_lift(x) for x in items]
    var = VARS[depth % len(VARS)]
    report = [] if report is None else report

    # pass 1 -- exact adjacent repetition, longest block wins
    out, i, n = [], 0, len(items)
    while i < n:
        best = None
        for p in range(1, (n - i) // 2 + 1):
            blk = items[i:i + p]
            reps = 1
            while items[i + reps * p:i + (reps + 1) * p] == blk:
                reps += 1
            if reps >= 2 and (best is None or reps * p > best[0]):
                best = (reps * p, p, reps)
        if best:
            span, p, reps = best
            body = (reduce_list(items[i:i + p], depth + 1, index_origin, report)
                    if p > 1 else [_lift(items[i])])
            cand = {"ic": {"var": None, "start": 1, "stop": reps, "body": body}}
            if expand(cand) == items[i:i + span]:
                out.append(cand)
                i += span
                continue
        out.append(_lift(items[i]))
        i += 1

    # pass 2 -- closed forms across equal-length blocks (paper eq.1 -> eq.2)
    best, best_sub = out, []
    for p in range(1, len(items) // 3 + 1):
        if len(items) % p:
            continue
        blocks = [items[q:q + p] for q in range(0, len(items), p)]
        if len(blocks) < 3:
            continue
        sub = []
        body = _fit_blocks(blocks, var, index_origin, sub)
        if body is None:
            continue
        cand = [{"ic": {"var": var, "start": index_origin,
                        "stop": index_origin + len(blocks) - 1, "body": body}}]
        if verify(cand, items) and _cost(cand) < _cost(best):
            best, best_sub = cand, sub
    if best is not out:
        report.extend(best_sub)
    return best


# --------------------------------------------------------------------------
# graphs (paper Table 2)
# --------------------------------------------------------------------------

def to_edsl(edges):
    """Edge list [(i,j),...] -> edge difference set list."""
    if not edges:
        return []
    hi = max(a for a, _ in edges)
    sets = [[] for _ in range(hi + 1)]
    for a, b in sorted(edges):
        sets[a].append(b - a)
    return [sorted(s) for s in sets[1:]]


def from_edsl(edsl):
    return [(i + 1, i + 1 + d) for i, s in enumerate(edsl) for d in s]


# --------------------------------------------------------------------------
# music (the fixed pipeline used in 2026/new experiments/*.nb)
# --------------------------------------------------------------------------

TICKS = {27: 2, 56: 4, 113: 8, 227: 16, 455: 32, 911: 64, 1823: 128,
         41: 3, 84: 6, 170: 12, 341: 24, 683: 48, 1367: 96, 2735: 192}


def normalize_ticks(x):
    """Map raw MIDI tick durations onto the 2^a*3^b lattice. Recurses."""
    if isinstance(x, list):
        return [normalize_ticks(y) for y in x]
    return TICKS.get(x, x)


def to_pitch_dur(triples):
    """{onset, duration, pitch} -> {pitch, duration}, ticks normalized.

    Onset is dropped because it is recoverable from the running duration sum;
    this is the Rest/@ then {#[[2]],#[[1]]}& /@ pair of steps in the notebooks.
    """
    return [[p, normalize_ticks(d)] for _, d, p in triples]


def restore_onsets(pairs, start=0):
    t, out = F(start), []
    for p, d in pairs:
        out.append([t, d, p])
        t += F(d)
    return out


# --------------------------------------------------------------------------

def pretty(node):
    """Render close to the notebooks' notation (EURO SIGN = concatenate)."""
    if isinstance(node, (int, str, F)):
        return str(node)
    if "list" in node:
        return "{" + ", ".join(pretty(x) for x in node["list"]) + "}"
    if "seq" in node:
        return "[" + ", ".join(pretty(x) for x in node["seq"]) + "]"
    ic = node["ic"]
    body = ", ".join(pretty(x) for x in ic["body"])
    if ic.get("var") is None:
        return f"€^{ic['stop']}[{body}]"
    return f"€_({ic['var']}|={ic['start']})^{ic['stop']}[{body}]"


def pretty_all(nodes):
    return "{" + ", ".join(pretty(n) for n in nodes) + "}"


def summarize(target, index_origin=0):
    report = []
    r = reduce_list(target, index_origin=index_origin, report=report)
    interp = [f for f in report if f.get("confirming", 1) == 0]
    return {"icn": r, "pretty": pretty_all(r), "lossless": verify(r, target),
            "compression":
                f"{_cost([_lift(x) for x in target])} -> {_cost(r)} chars",
            "fits": report,
            "interpolating_fits": len(interp),
            "warning": ("some closed forms interpolate exactly and predict "
                        "nothing beyond the given data") if interp else None}


if __name__ == "__main__":
    import sys
    data = json.loads(sys.stdin.read())
    if isinstance(data, dict):
        origin = data.get("index_origin", 0)
        if "edges" in data:
            target = to_edsl([tuple(e) for e in data["edges"]])
        elif "triples" in data:
            target = to_pitch_dur(data["triples"])
        elif data.get("normalize"):
            target = normalize_ticks(data["items"])
        else:
            target = data["items"]
    else:
        target, origin = data, 0
    print(json.dumps(summarize(target, origin), indent=2))
