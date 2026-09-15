"""Indexed Concatenation: model, parser and expander.

Mirrors the Wolfram implementation in SSSiCv102.wl. An IC object carries the
elements to repeat and an iterator; expansion tables the elements over the index
and splices the results into the surrounding list, exactly as `iC` does:

    iC[x__, iter] := Sequence @@ (Join @@ Table[{x}, iter])

Two behaviours from the paper are load-bearing and preserved here: an IC with no
arguments vanishes, and so does one whose index range is empty -- which is why
€^0[...] and a negative end value are legal rather than errors.

Input syntax (ASCII alias IC for the € sign):

    €3[{32,60}]              repeat three times
    €(i=0..3)[{32, 60+2*i}]  index i running 0 to 3
    €(i..4)[{8, 1/6*(i^3-13*i)+81}]   start defaults to 1
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from fractions import Fraction

import sympy

EURO = "€"


class Seq(list):
    """A [ ... ] group: a *vanishing* delimiter.

    Section 4 of the paper treats square brackets as a sequence delimiter that
    disappears when the sequence sits inside another object, so its contents
    splice into the parent. Curly braces keep their nesting. Equation 21 turns
    on exactly this difference.
    """


@dataclass
class Iter:
    """An IC index: a bare repeat count, or a variable over a range."""
    var: str | None
    start: int
    stop: object          # int, or a sympy expression in an enclosing variable

    @property
    def is_count(self) -> bool:
        return self.var is None

    def render(self) -> str:
        if self.is_count:
            return f"{EURO}{self.stop}"
        if self.start == 1:
            return f"{EURO}({self.var}..{self.stop})"
        return f"{EURO}({self.var}={self.start}..{self.stop})"


@dataclass
class IC:
    """An indexed concatenation: repeat `args` over `it`, splicing the result."""
    args: list
    it: Iter

    def render(self) -> str:
        return f"{self.it.render()}[{', '.join(_render(a) for a in self.args)}]"


def _render(v) -> str:
    if isinstance(v, IC):
        return v.render()
    if isinstance(v, Seq):
        return "[" + ", ".join(_render(x) for x in v) + "]"
    if isinstance(v, (list, tuple)):
        return "{" + ", ".join(_render(x) for x in v) + "}"
    if isinstance(v, Fraction):
        return str(v.numerator) if v.denominator == 1 else f"{v.numerator}/{v.denominator}"
    return str(v)


def render(tree) -> str:
    return _render(tree)


# --------------------------------------------------------------------------
# Expansion
# --------------------------------------------------------------------------

class ICError(ValueError):
    pass


def _evaluate(value, env: dict):
    """Resolve a leaf to a number under the current index bindings."""
    if isinstance(value, (int, Fraction)):
        return value
    if isinstance(value, sympy.Expr):
        out = value.subs(env)
        if out.free_symbols:
            raise ICError(f"unbound index in {value}; bound: {sorted(map(str, env))}")
        r = sympy.nsimplify(out)
        if r.is_Integer:
            return int(r)
        if r.is_Rational:
            return Fraction(int(r.p), int(r.q))
        return float(r)
    return value


def expand(node, env: dict | None = None) -> list:
    """Fully expand a tree, returning plain nested lists of numbers.

    A list expands elementwise, with any IC child splicing its rows in place --
    that is what makes the [] delimiters of the paper 'vanishing'.
    """
    env = env or {}

    if isinstance(node, IC):
        it = node.it
        stop = _evaluate(it.stop, env)
        stop = int(stop)
        out: list = []
        # An empty or reversed range yields nothing: the paper notes that a 0 or
        # negative end value is legal, and the subsequence is simply omitted.
        bindings = ([{}] * max(0, stop) if it.is_count
                    else [{sympy.Symbol(it.var): i}
                          for i in range(it.start, stop + 1)])
        for b in bindings:
            inner = {**env, **b}
            for a in node.args:
                _emit(out, a, inner)
        return out

    if isinstance(node, Seq):
        out = Seq()
        for item in node:
            _emit(out, item, env)
        return out

    if isinstance(node, (list, tuple)):
        out = []
        for item in node:
            _emit(out, item, env)
        return out

    return _evaluate(node, env)


def _emit(out: list, item, env: dict) -> None:
    """Append one expanded item, splicing it if its delimiter vanishes."""
    value = expand(item, env)
    if isinstance(item, IC) or isinstance(value, Seq):
        out.extend(value)
    else:
        out.append(value)


# --------------------------------------------------------------------------
# Parsing
# --------------------------------------------------------------------------

_ITER_VAR = re.compile(r"\(\s*([A-Za-z]\w*)\s*(?:=\s*(-?\d+)\s*)?\.\.\s*([^)]+?)\s*\)")
_ITER_NUM = re.compile(r"(\d+)")


def parse(text: str):
    """Parse an IC expression into a tree of lists, IC objects and numbers."""
    s = text.replace("IC", EURO).replace("\\[Euro]", EURO).strip()
    pos = 0

    def skip():
        nonlocal pos
        while pos < len(s) and s[pos] in " \t\r\n":
            pos += 1

    def parse_seq(closer: str, seq: bool = False) -> list:
        nonlocal pos
        items: list = Seq() if seq else []
        while True:
            skip()
            if pos >= len(s):
                if closer:
                    raise ICError(f"missing closing {closer!r}")
                break
            if closer and s[pos] == closer:
                pos += 1
                break
            if s[pos] == ",":
                pos += 1
                continue
            items.append(parse_one())
        return items

    def parse_one():
        nonlocal pos
        skip()
        c = s[pos]
        if c == EURO:
            pos += 1
            skip()
            m = _ITER_VAR.match(s, pos)
            if m:
                pos = m.end()
                var, start, stop = m.group(1), m.group(2), m.group(3)
                it = Iter(var, int(start) if start else 1, _sym(stop))
            else:
                m = _ITER_NUM.match(s, pos)
                if not m:
                    raise ICError(f"{EURO} needs a count or (i=a..b) at {pos}")
                pos = m.end()
                it = Iter(None, 1, int(m.group(1)))
            skip()
            if pos >= len(s) or s[pos] not in "[{":
                raise ICError(f"{EURO} needs a bracketed body at {pos}")
            opener, pos = s[pos], pos + 1
            # The bracket right after € delimits the IC's arguments; it is not
            # itself a vanishing sequence.
            body = parse_seq("]" if opener == "[" else "}")
            return IC(list(body), it)
        if c in "[{":
            pos += 1
            return parse_seq("]" if c == "[" else "}", seq=(c == "["))
        # a scalar: read to the next top-level delimiter, hand it to sympy
        depth, start = 0, pos
        while pos < len(s):
            ch = s[pos]
            if ch in "([{":
                depth += 1
            elif ch in ")]}":
                if depth == 0:
                    break
                depth -= 1
            elif ch == "," and depth == 0:
                break
            elif ch == EURO and depth == 0:
                break
            pos += 1
        tok = s[start:pos].strip()
        if not tok:
            raise ICError(f"empty token at {start}")
        return _sym(tok)

    tree = parse_seq("")
    return tree[0] if len(tree) == 1 else tree


def _sym(tok: str):
    """A number stays exact; anything with a variable becomes a sympy expr."""
    tok = tok.strip()
    if re.fullmatch(r"-?\d+", tok):
        return int(tok)
    if re.fullmatch(r"-?\d+\s*/\s*-?\d+", tok):
        return Fraction(tok.replace(" ", ""))
    try:
        return sympy.sympify(tok.replace("^", "**"), rational=True)
    except Exception as exc:
        raise ICError(f"cannot parse {tok!r}: {exc}") from exc


def expand_text(text: str) -> list:
    return expand(parse(text))


# --------------------------------------------------------------------------
# spans -- which expanded rows each IC node covers
# --------------------------------------------------------------------------

def _rows_of(node) -> int:
    """How many rows a node contributes once expanded."""
    return len(expand([node])) if isinstance(node, IC) else 1


def spans(tree, limit: int = 3000) -> list[dict]:
    """Flat list of {depth, start, end, label, text} over expanded row indices.

    This is what lets the score colour an IC's blocks: each node reports the
    stretch of notes it produces, and nesting shows as depth. Inner nodes are
    reported once per iteration of their parent, so every copy of a repeating
    figure is marked, not just the first. `id` is stable across the copies of
    one node, so hovering any of them can light up all of them.
    """
    out: list[dict] = []
    counter = [0]

    def walk(items, pos: int, depth: int, env: dict, path: str) -> int:
        for k, item in enumerate(items):
            if not isinstance(item, IC):
                pos += 1
                continue
            here = f"{path}.{k}"
            it = item.it
            try:
                stop = int(_evaluate(it.stop, env))
            except Exception:
                pos += 1
                continue
            bindings = ([{}] * max(0, stop) if it.is_count
                        else [{sympy.Symbol(it.var): i}
                              for i in range(it.start, stop + 1)])
            start = pos
            for b in bindings:
                inner = {**env, **b}
                if len(out) < limit:
                    pos = walk(item.args, pos, depth + 1, inner, here)
                else:                       # stop descending, still advance
                    pos += len(expand(list(item.args), inner))
            if len(out) < limit and pos > start:
                out.append({
                    "id": here, "depth": depth,
                    "start": start, "end": pos - 1, "rows": pos - start,
                    "label": it.render(), "text": render(item),
                    "reps": stop if it.is_count else stop - it.start + 1,
                })
        return pos

    walk(tree if isinstance(tree, list) else [tree], 0, 0, {}, "r")
    out.sort(key=lambda s: (s["depth"], s["start"]))
    return out
