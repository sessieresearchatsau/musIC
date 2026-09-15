"""Read and write set literals.

Accepts what you would paste out of a notebook -- {{0,32,60},{1/2,32,60}} with
optional Fraction/FractionBox wrappers and Mathematica's box syntax -- as well
as plain Python lists. Emits either dialect.
"""
from __future__ import annotations

import re
from fractions import Fraction

Row = tuple

_NUM = re.compile(r"-?\d+(?:\.\d+)?")


# Wrappers a notebook copy carries along. Each is replaced by its first
# argument: StyleBox[{62,24}, Background->...] -> {62,24}.
_WRAPPERS = ("StyleBox", "RowBox", "TagBox", "DisplayForm", "InterpretationBox",
             "TemplateBox", "BoxData")


def _match_bracket(s: str, i: int) -> int:
    """Index of the bracket closing the one at s[i]. -1 if unbalanced."""
    pairs = {"[": "]", "{": "}", "(": ")"}
    stack = [pairs[s[i]]]
    j = i + 1
    while j < len(s) and stack:
        c = s[j]
        if c in pairs:
            stack.append(pairs[c])
        elif c in "]})":
            if c != stack[-1]:
                return -1
            stack.pop()
        j += 1
    return j - 1 if not stack else -1


def _unwrap(text: str) -> str:
    """Replace Head[first, ...] with first, for each known wrapper head."""
    for head in _WRAPPERS:
        while True:
            i = text.find(head + "[")
            if i < 0:
                break
            open_at = i + len(head)
            close_at = _match_bracket(text, open_at)
            if close_at < 0:
                text = text[:i] + text[open_at + 1:]   # unbalanced; drop head
                continue
            inner = text[open_at + 1:close_at]
            # keep only the first top-level argument
            depth, cut = 0, len(inner)
            for k, c in enumerate(inner):
                if c in "[{(":
                    depth += 1
                elif c in "]})":
                    depth -= 1
                elif c == "," and depth == 0:
                    cut = k
                    break
            text = text[:i] + inner[:cut] + text[close_at + 1:]
    return text


def _strip_boxes(text: str) -> str:
    """Flatten the box forms a notebook copy carries along."""
    # FractionBox["11", "8"] -> 11/8, before the generic unwrapping runs.
    text = re.sub(r'FractionBox\[\s*"?(-?[\d.]+)"?\s*,\s*"?(-?[\d.]+)"?\s*\]',
                  r"\1/\2", text)
    text = _unwrap(text)
    # option tails that survive, e.g. a bare Background->RGBColor[...]
    text = re.sub(r"\w+\s*->\s*\w+\[[^\]]*\]", "", text)
    text = re.sub(r'"\s*,\s*"', ",", text)
    return text.replace('"', "")


def _num(tok: str):
    tok = tok.strip()
    if "/" in tok:
        return Fraction(tok)
    if "." in tok:
        return Fraction(tok).limit_denominator(10**6)
    return int(tok)


def parse_set(text: str):
    """Parse a set literal into nested lists of numbers.

    Returns a list of rows (each a tuple), or a list of groups of rows when the
    literal is nested one level deeper, as the EDSL-shaped encodings are.
    """
    s = _strip_boxes(text).strip()
    s = s.replace("[", "{").replace("]", "}")
    if not s:
        return []

    # Recursive-descent over braces; anything else is a number token.
    pos = 0

    def parse_value():
        nonlocal pos
        while pos < len(s) and s[pos] in " \t\r\n,":
            pos += 1
        if pos < len(s) and s[pos] == "{":
            pos += 1
            items = []
            while pos < len(s):
                while pos < len(s) and s[pos] in " \t\r\n,":
                    pos += 1
                if pos < len(s) and s[pos] == "}":
                    pos += 1
                    break
                items.append(parse_value())
            return items
        m = re.match(r"-?\d+(?:\s*/\s*-?\d+)?(?:\.\d+)?", s[pos:])
        if not m:
            raise ValueError(f"cannot parse at offset {pos}: {s[pos:pos+24]!r}")
        pos += m.end()
        return _num(m.group().replace(" ", ""))

    value = parse_value()

    # Peel a redundant outer wrapper: {{{...},{...}}} -> {{...},{...}}
    while (isinstance(value, list) and len(value) == 1
           and isinstance(value[0], list)
           and all(isinstance(x, list) for x in value[0])):
        value = value[0]

    if not isinstance(value, list):
        return [(value,)]

    def depth(v):
        return 1 + max((depth(x) for x in v if isinstance(x, list)), default=0) \
            if isinstance(v, list) else 0

    d = depth(value)
    if d <= 1:                       # flat list of numbers -> one column
        return [(v,) for v in value]
    if d == 2:                       # list of rows
        return [tuple(r) if isinstance(r, list) else (r,) for r in value]
    # list of groups of rows
    return [[tuple(r) if isinstance(r, list) else (r,) for r in g] for g in value]


def _fmt(v) -> str:
    if isinstance(v, Fraction):
        return str(v.numerator) if v.denominator == 1 else f"{v.numerator}/{v.denominator}"
    return str(v)


def to_mathematica(rows, indent: str = "") -> str:
    """Render as {{a,b},{c,d}} -- paste-ready for a notebook."""
    def render(v):
        if isinstance(v, (list, tuple)):
            return "{" + ",".join(render(x) for x in v) + "}"
        return _fmt(v)
    return indent + render(list(rows))


def to_python(rows) -> str:
    def render(v):
        if isinstance(v, (list, tuple)):
            return "[" + ", ".join(render(x) for x in v) + "]"
        return _fmt(v)
    return render(list(rows))
