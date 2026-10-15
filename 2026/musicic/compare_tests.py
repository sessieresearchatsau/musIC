"""concat.py against reduce.py on the reference sets: both must be lossless.
Run: uv run python compare_tests.py"""
import re

from musicic.core.concat import concatenate
from musicic.core.ic import expand
from musicic.core.reduce import reduce_rows

EXAMPLES = ("/Users/derekrenck/Documents/GitHub/musIC/2026/musicic/docs/"
            "concatenation-examples.md")

fails = []


def check(name, got, want):
    if got != want:
        fails.append(f"{name}: got {got!r} want {want!r}")


def reference_set(doc, n):
    """The rows of set n, read from its first code block."""
    block = doc.split(f"### {n}. Set {n}")[1].split("```")[1]
    return [tuple(int(x) for x in re.findall(r"-?\d+", m))
            for m in re.findall(r"\{([^{}]*)\}", block)]


_doc = open(EXAMPLES).read()
for n in range(1, 7):
    rows = reference_set(_doc, n)
    check(f"set {n} has rows", len(rows) > 0, True)

    r = reduce_rows(rows)
    check(f"set {n} reduce verified", r.verified, True)
    check(f"set {n} reduce expands", [tuple(x) for x in expand(r.tree)], rows)

    c = concatenate(rows)
    check(f"set {n} concat verified", c.verified, True)
    check(f"set {n} concat expands", [tuple(x) for x in expand(c.tree)], rows)

    # Both must reproduce the same input, so they must agree with each other.
    check(f"set {n} reduce and concat agree",
          [tuple(x) for x in expand(r.tree)],
          [tuple(x) for x in expand(c.tree)])

print("\n".join(fails) if fails else "all compare checks pass")
raise SystemExit(1 if fails else 0)
