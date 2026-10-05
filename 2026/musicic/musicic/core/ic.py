"""Indexed Concatenation: model, parser and expander.

Mirrors the Wolfram implementation in SSSiCv102.wl. An IC object carries the
elements to repeat and an iterator; expansion tables the elements over the index
and splices the results into the surrounding list, exactly as `iC` does:

    iC[x__, iter] := Sequence @@ (Join @@ Table[{x}, iter])

Two behaviours from the paper are load-bearing and preserved here: an IC with no
arguments vanishes, and so does one whose index range is empty -- which is why
€^0[...] and a negative end value are legal rather than errors.

Input syntax (ASCII alias IC for the € sign):

    €(3)[{32,60}]            repeat three times (€3[...] is read the same)
    €(n)[{32,60}]            repeat n times, n an enclosing index; €(0) makes none
    €(i=0..3)[{32, 60+2*i}]  index i running 0 to 3
    €(i..4)[{8, 1/6*(i^3-13*i)+81}]   start defaults to 1
    €(i=0,n=3)[{32, 60+2*i}]  the same range written as the sum is: i from 0
                             up to n = 3, and n can be used inside the body
    €(i,0,3)[{32, 60+2*i}]   the same again as Mathematica's Table iterator:
                             variable, first, last. First and last may use an
                             enclosing index, as in €(i,0,3)[€(j,i,3)[...]]
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
    start: object         # int, or a sympy expression in an enclosing variable
    stop: object          # likewise
    upper: str | None = None   # the name the stop was given, as n in (i=0,n=3)
    table: bool = False   # written (i,0,3), and rendered back that way

    @property
    def is_count(self) -> bool:
        return self.var is None

    def render(self) -> str:
        if self.is_count:
            return f"{EURO}({self.stop})"
        if self.table:
            return f"{EURO}({self.var},{self.start},{self.stop})"
        if self.upper:
            return f"{EURO}({self.var}={self.start},{self.upper}={self.stop})"
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
            free = ", ".join(sorted(map(str, out.free_symbols)))
            have = ", ".join(sorted(map(str, env))) or "none"
            raise ICError(f"{free} is not set here: no enclosing IC sets it "
                          f"(indices in scope: {have})")
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
        out: list = []
        for b in _bindings(node.it, env):
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


def _bindings(it: Iter, env: dict) -> list[dict]:
    """One index binding per repetition, with the limits resolved under the
    enclosing indices. An empty or reversed range yields nothing: the paper
    notes that a 0 or negative end value is legal, and the subsequence is
    simply omitted."""
    stop = int(_evaluate(it.stop, env))
    if it.is_count:
        return [{}] * max(0, stop)
    start = int(_evaluate(it.start, env))
    # A named upper limit is in scope in the body, as n is under a sum sign.
    top = {sympy.Symbol(it.upper): stop} if it.upper else {}
    return [{**top, sympy.Symbol(it.var): i} for i in range(start, stop + 1)]


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
# (i, 0, 3): variable, first, last, as a Mathematica Table iterator. The limits
# are read to the next top-level comma, so they may be expressions in an
# enclosing index; a comma inside parentheses does not split them.
_ITER_TABLE = re.compile(r"\(\s*([A-Za-z]\w*)\s*,")
# (i=0, n=3) or (i=0, 3): the lower and upper limits of a sum, comma-separated.
_ITER_SUM = re.compile(
    r"\(\s*([A-Za-z]\w*)\s*=\s*(-?\d+)\s*,\s*(?:([A-Za-z]\w*)\s*=\s*)?([^)]+?)\s*\)")
_ITER_NUM = re.compile(r"(\d+)")


def _normalize(text: str) -> tuple[str, list[int]]:
    """Fold the aliases IC and \\[Euro] to €, keeping where each character of
    the result came from, so a position the parser reports can be pointed at in
    the text the person actually typed."""
    out, orig, i = [], [], 0
    while i < len(text):
        for alias in ("\\[Euro]", "IC"):
            if text.startswith(alias, i):
                out.append(EURO); orig.append(i); i += len(alias)
                break
        else:
            out.append(text[i]); orig.append(i); i += 1
    orig.append(len(text))
    return "".join(out), orig


def parse(text: str, where: dict | None = None):
    """Parse an IC expression into a tree of lists, IC objects and numbers.

    Pass a dict as `where` to learn where each list and IC came from in `text`:
    it is filled with id(node) -> (start, end), and for an IC (start, body, end)
    where `body` is just past its opening bracket. Offsets are into `text` as
    given, aliases and all.
    """
    s, orig = _normalize(text)
    pos = 0

    def mark(node, start, *rest):
        if where is not None:
            where[id(node)] = (orig[start],) + tuple(orig[p - 1] + 1 for p in rest)
        return node

    def skip():
        nonlocal pos
        while pos < len(s) and s[pos] in " \t\r\n":
            pos += 1

    def parse_seq(closer: str, seq: bool = False, opened: int | None = None) -> list:
        nonlocal pos
        items: list = Seq() if seq else []
        # Positions in messages count characters of the text as typed, from 1.
        where_ = lambda p: orig[min(p, len(orig) - 1)] + 1           # noqa: E731
        opener = {"]": "[", "}": "{"}.get(closer, "")
        while True:
            skip()
            if pos >= len(s):
                if closer:
                    raise ICError(f"the {opener!r} at character {where_(opened)} is never closed: "
                                  f"add a {closer!r}" if opened is not None
                                  else f"missing closing {closer!r}")
                break
            if closer and s[pos] == closer:
                pos += 1
                break
            if s[pos] in "]})":
                # A bracket that closes something other than what is open.
                if opened is None:
                    raise ICError(f"the {s[pos]!r} at character {where_(pos)} closes nothing")
                raise ICError(f"the {opener!r} at character {where_(opened)} needs a {closer!r}, "
                              f"but the {s[pos]!r} at character {where_(pos)} comes first")
            if s[pos] == ",":
                pos += 1
                continue
            items.append(parse_one())
        return items

    def table_limits() -> list[str]:
        """The comma-separated limits of (i, a, b), up to its closing paren."""
        nonlocal pos
        parts, depth, start = [], 0, pos
        while pos < len(s):
            ch = s[pos]
            if ch == "(":
                depth += 1
            elif ch == ")":
                if depth == 0:
                    parts.append(s[start:pos].strip()); pos += 1
                    return parts
                depth -= 1
            elif ch == "," and depth == 0:
                parts.append(s[start:pos].strip()); start = pos + 1
            pos += 1
        raise ICError(f"{EURO}( is missing its closing parenthesis")

    def parse_one():
        nonlocal pos
        skip()
        c = s[pos]
        at = pos
        if c == EURO:
            pos += 1
            skip()
            m = _ITER_VAR.match(s, pos)
            ms = None if m else _ITER_SUM.match(s, pos)
            mt = None if m or ms else _ITER_TABLE.match(s, pos)
            if mt:
                pos = mt.end()
                limits = table_limits()
                if len(limits) != 2:
                    raise ICError(f"{EURO}(i, first, last) needs a first and a last "
                                  f"value; got {len(limits)} after {mt.group(1)!r}")
                it = Iter(mt.group(1), _sym(limits[0]), _sym(limits[1]), table=True)
            elif m:
                pos = m.end()
                var, start, stop = m.group(1), m.group(2), m.group(3)
                it = Iter(var, int(start) if start else 1, _sym(stop))
            elif ms:
                pos = ms.end()
                var, start, upper, stop = ms.groups()
                if upper == var:
                    raise ICError(f"the upper limit cannot share the index name {var!r}")
                it = Iter(var, int(start), _sym(stop), upper)
            elif pos < len(s) and s[pos] == "(":
                # €(2) or €(n): a count, which may be a formula in an enclosing
                # index. It is worked out when expanded; 0 or less makes no copies.
                pos += 1
                limits = table_limits()
                if len(limits) != 1 or not limits[0]:
                    raise ICError(f"{EURO}( ) holds a count like (2) or (n), or an index: "
                                  f"(i,a,b), (i=a..b) or (i=a,n=b)")
                it = Iter(None, 1, _sym(limits[0]))
            else:
                m = _ITER_NUM.match(s, pos)
                if not m:
                    raise ICError(f"{EURO} needs a count (2) or (n), or an index (i,a,b), "
                                  f"(i=a..b) or (i=a,n=b), at {pos}")
                pos = m.end()
                it = Iter(None, 1, int(m.group(1)))      # €2: the older way to write €(2)
            skip()
            if pos >= len(s) or s[pos] not in "[{":
                raise ICError(f"{EURO} needs a bracketed body at {pos}")
            opener, pos = s[pos], pos + 1
            head = pos
            # The bracket right after € delimits the IC's arguments; it is not
            # itself a vanishing sequence.
            body = parse_seq("]" if opener == "[" else "}", opened=head - 1)
            return mark(IC(list(body), it), at, head, pos)
        if c in "[{":
            pos += 1
            return mark(parse_seq("]" if c == "[" else "}", seq=(c == "["), opened=at), at, pos)
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
        # A number against a letter or bracket multiplies. Said outright, since
        # Python would read 13j as an imaginary number, not 13 times j.
        tok = re.sub(r"(\d)\s*(?=[A-Za-z_(])", r"\1*", tok)
        return parse_expr(tok.replace("^", "**"), transformations=_READ)
    except Exception as exc:
        raise ICError(f"cannot read {tok!r} as a number or a formula in the index; "
                      f"write it like 60+2i, 60+2*i or 2(i+1)") from exc


# Formulas are read as Mathematica writes them: 2i and 2(i+1) multiply, as
# does a space between factors (2 i). Letters stay whole -- ij is one name, not
# i times j -- and decimals are kept exact.
from sympy.parsing.sympy_parser import (  # noqa: E402
    implicit_multiplication, parse_expr, rationalize, standard_transformations)

_READ = standard_transformations + (implicit_multiplication, rationalize)


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
                bindings = _bindings(it, env)
            except Exception:
                pos += 1
                continue
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
                    "reps": len(bindings),
                })
        return pos

    walk(tree if isinstance(tree, list) else [tree], 0, 0, {}, "r")
    out.sort(key=lambda s: (s["depth"], s["start"]))
    return out


# --------------------------------------------------------------------------
# sources -- where in the text each expanded row and each IC came from
# --------------------------------------------------------------------------

def sources(text: str) -> dict:
    """Tie the expansion of `text` back to the characters that produced it.

    `rows[i]` is the (start, end) of the literal that became expanded row i, or
    None where a row has no literal of its own. One literal inside an IC yields
    a row per repetition, so several rows can share a source. `blocks` gives
    each IC node, under the same `id` that `spans` uses, its (start, body, end).

    `parts` are the top-level pieces of the expression, in order -- the items
    of `{€2[{16,67}], {32,69}}` are `€2[{16,67}]` and `{32,69}` -- each with its
    (start, end) and how many rows it expands to. They are what a next level
    of concatenation works with.

    Rows are walked exactly as `expand` splices them, so the two stay the same
    length; if they ever did not, `rows` is empty rather than misaligned.
    """
    where: dict = {}
    tree = parse(text, where)
    rows: list = []

    def emit(item, env):
        if isinstance(item, IC):
            for b in _bindings(item.it, env):
                for a in item.args:
                    emit(a, {**env, **b})
        elif isinstance(item, Seq):
            for a in item:
                emit(a, env)
        else:
            rows.append(where.get(id(item)))

    parts: list = []
    for a in [tree] if isinstance(tree, IC) else tree if isinstance(tree, list) else []:
        before = len(rows)
        emit(a, {})
        src = where.get(id(a))
        parts.append({"src": [src[0], src[-1]] if src else None,
                      "rows": len(rows) - before})

    blocks: list = []

    def walk(items, path):                 # the same path scheme as spans()
        for k, item in enumerate(items):
            if isinstance(item, IC):
                here = f"{path}.{k}"
                blocks.append({"id": here, "src": where.get(id(item)),
                               "label": item.it.render()})
                walk(item.args, here)

    walk(tree if isinstance(tree, list) else [tree], "r")
    if len(rows) != len(expand(tree)):
        rows, parts = [], []
    return {"rows": rows, "blocks": blocks, "parts": parts}


# --------------------------------------------------------------------------
# writing an expression out -- as typeset math, and as Mathematica code
# --------------------------------------------------------------------------

def _blocks_order(tree) -> dict:
    """IC node id -> its place in parse order, walking as sources() does, so
    colours in an export match the notebook's."""
    order: dict = {}

    def walk(items):
        for item in items:
            if isinstance(item, IC):
                order[id(item)] = len(order)
                walk(item.args)

    walk(tree if isinstance(tree, list) else [tree])
    return order


def _tex_leaf(v) -> str:
    if isinstance(v, Fraction):
        return str(v.numerator) if v.denominator == 1 else rf"\tfrac{{{v.numerator}}}{{{v.denominator}}}"
    if isinstance(v, sympy.Expr):
        return sympy.latex(v)
    return str(v)


def render_latex(tree, colors: list[str] | None = None) -> str:
    """The expression as LaTeX math: € with its index under and its last value
    over, as the paper writes it, and sets in braces. Commas allow a line break,
    so a long level wraps instead of running off the page. With `colors`, each
    € takes the colour of its block, in parse order (xcolor names or HTML hex)."""
    order = _blocks_order(tree)

    def euro(node) -> str:
        sym = r"\text{\euro}"
        if colors:
            c = colors[order.get(id(node), 0) % len(colors)]
            sym = rf"\textcolor[HTML]{{{c}}}{{{sym}}}"
        it = node.it
        if it.is_count:
            return rf"\mathop{{{sym}}}\nolimits^{{{_tex_leaf(it.stop)}}}"
        top = _tex_leaf(it.stop)
        if it.upper:
            top = rf"{it.upper}={top}"
        return rf"\mathop{{{sym}}}\limits_{{{it.var}={_tex_leaf(it.start)}}}^{{{top}}}"

    def go(v) -> str:
        if isinstance(v, IC):
            return euro(v) + r"\bigl[" + r",\allowbreak ".join(go(a) for a in v.args) + r"\bigr]"
        if isinstance(v, Seq):
            return "[" + r",\allowbreak ".join(go(a) for a in v) + "]"
        if isinstance(v, (list, tuple)):
            inner = ",".join(go(a) for a in v)
            # a row of numbers stays together; a list of rows may break
            if any(isinstance(a, (list, tuple, IC)) for a in v):
                inner = r",\allowbreak ".join(go(a) for a in v)
            return r"\{" + inner + r"\}"
        return _tex_leaf(v)

    return go(tree)


def render_wl(tree) -> str:
    """The expression as Wolfram Language, using iC from SSSiCv102.wl:
    iC[x__, iter] with iter a count, {i, n} or {i, a, b}. A named upper limit,
    as n in €(i=0,n=7), is bound with With so the body can use it."""
    from sympy.printing.mathematica import mathematica_code

    def leaf(v) -> str:
        if isinstance(v, Fraction):
            return str(v.numerator) if v.denominator == 1 else f"{v.numerator}/{v.denominator}"
        if isinstance(v, sympy.Expr):
            return mathematica_code(v)
        return str(v)

    def go(v) -> str:
        if isinstance(v, IC):
            it, body = v.it, ", ".join(go(a) for a in v.args)
            if it.is_count:
                return f"iC[{body}, {leaf(it.stop)}]"
            if it.upper:
                return (f"With[{{{it.upper} = {leaf(it.stop)}}}, "
                        f"iC[{body}, {{{it.var}, {leaf(it.start)}, {it.upper}}}]]")
            return f"iC[{body}, {{{it.var}, {leaf(it.start)}, {leaf(it.stop)}}}]"
        if isinstance(v, Seq):
            return "Sequence[" + ", ".join(go(a) for a in v) + "]"
        if isinstance(v, (list, tuple)):
            return "{" + ", ".join(go(a) for a in v) + "}"
        return leaf(v)

    return go(tree)


# --------------------------------------------------------------------------
# rearranging the columns of every set in an expression
# --------------------------------------------------------------------------

def _split_top(inner: str) -> tuple[list[str], str]:
    """The comma-separated entries of a set's text, and the separator used."""
    parts, depth, start, spaced = [], 0, 0, False
    for k, ch in enumerate(inner):
        if ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif ch == "," and depth == 0:
            parts.append(inner[start:k])
            spaced = spaced or inner[k + 1:k + 2] == " "
            start = k + 1
    parts.append(inner[start:])
    return [p.strip() for p in parts], ", " if spaced else ","


def reorder(text: str, order: list[int]) -> str:
    """`text` with every set's entries rearranged: entry k of the result is
    entry order[k] of the original. {16, 60+i} under (1, 0) is {60+i, 16}.

    Only sets -- lists of plain values as wide as `order` -- are touched, the
    € heads and the lists that hold sets are left alone, and each entry keeps
    the text it was written with, formulas and all. Sets that vanish (inside
    €0) are rearranged too, so the whole cell stays in one column order.
    """
    width = len(order)
    if sorted(order) != list(range(width)):
        raise ICError(f"{order} is not a rearrangement of {width} columns")
    where: dict = {}
    tree = parse(text, where)
    spans_: list[tuple[int, int]] = []

    def walk(node):
        if isinstance(node, IC):
            for a in node.args:
                walk(a)
        elif isinstance(node, (list, tuple)):
            flat = not any(isinstance(x, (list, tuple, IC)) for x in node)
            if flat and not isinstance(node, Seq) and len(node) == width and id(node) in where:
                spans_.append(where[id(node)])
            else:
                for x in node:
                    walk(x)

    walk(tree)
    out = text
    for start, end in sorted(spans_, reverse=True):     # from the end, so offsets hold
        body = out[start:end]
        if len(body) < 2 or body[0] not in "{[" or body[-1] not in "}]":
            continue
        entries, sep = _split_top(body[1:-1])
        if len(entries) != width:
            continue
        out = out[:start] + body[0] + sep.join(entries[c] for c in order) + body[-1] + out[end:]
    return out
