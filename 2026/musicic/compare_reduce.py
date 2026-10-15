"""concat.py against reduce.py on the same sets: the paper's networks
(docs/networks), the made-up stress forms, and the reference sets of
docs/concatenation-examples.md. Kept apart from the concat tests, to run when
the two are to be compared.

Run: uv run python compare_reduce.py [--save results.json] [--limit SECONDS]

For each set and each reducer: rows and numbers written (as network_tests.py
counts them), whether it expands back exactly, and the time it took. Each run
gets the time limit in a process of its own."""
import json
import multiprocessing as mp
import re
import sys
import time
from pathlib import Path

HERE = Path(__file__).parent
sys.path.insert(0, str(HERE / "docs" / "networks"))

from paper_forms import FORMS, rows as paper_rows     # noqa: E402
from stress_forms import STRESS                       # noqa: E402

from musicic.core.ic import expand, parse, render      # noqa: E402


def numbers(tree) -> int:
    return len(re.findall(r"\d+|[A-Za-z]\w*", render(tree)))


def sets() -> dict:
    out = {name: paper_rows(name) for name in FORMS}
    for name, text in STRESS.items():
        out[f"stress:{name}"] = [tuple(x) for x in expand(parse(text))]
    doc = (HERE / "docs" / "concatenation-examples.md").read_text()
    for n in range(1, 7):
        block = doc.split(f"### {n}. Set {n}")[1].split("```")[1]
        out[f"ref{n}"] = [tuple(int(x) for x in re.findall(r"-?\d+", m))
                          for m in re.findall(r"\{([^{}]*)\}", block)]
    return out


def _run(which, rows, q):
    t = time.time()
    if which == "concat":
        from musicic.core.concat import concatenate, leaves
        r = concatenate(rows)
        tree, size = r.tree, leaves(r.tree)
    else:
        from musicic.core.concat import leaves
        from musicic.core.reduce import reduce_rows
        r = reduce_rows(rows)
        tree, size = r.tree, leaves(r.tree)
    ok = [tuple(x) for x in expand(tree)] == [tuple(x) for x in rows]
    q.put({"rows": size, "numbers": numbers(tree), "lossless": ok,
           "seconds": round(time.time() - t, 2)})


def run(which, rows, limit) -> dict:
    q = mp.Queue()
    p = mp.Process(target=_run, args=(which, rows, q))
    p.start()
    p.join(limit)
    if p.is_alive():
        p.terminate()
        return {"timeout": True}
    return q.get() if not q.empty() else {"crash": True}


def cell(d) -> str:
    if d.get("timeout"):
        return "timeout"
    if d.get("crash"):
        return "crash"
    return f"{d['rows']:>4} rows {d['numbers']:>5} nums{'' if d['lossless'] else ' LOSSY'}"


def main():
    args = sys.argv[1:]
    save = args[args.index("--save") + 1] if "--save" in args else None
    limit = float(args[args.index("--limit") + 1]) if "--limit" in args else 120
    out = {}
    wins = {"concat": 0, "reduce": 0, "tie": 0}
    for name, rows in sets().items():
        c, r = run("concat", rows, limit), run("reduce", rows, limit)
        out[name] = {"n": len(rows), "concat": c, "reduce": r}
        key = (lambda d: (d["rows"], d["numbers"]) if d.get("lossless") else (1e9, 1e9))
        kc, kr = key(c), key(r)
        w = "tie" if kc == kr else ("concat" if kc < kr else "reduce")
        wins[w] += 1
        print(f"{name:24} n={len(rows):<5} concat {cell(c):26} reduce {cell(r):26} -> {w}")
    print(f"\nsmaller: concat {wins['concat']}, reduce {wins['reduce']}, tie {wins['tie']}")
    if save:
        Path(save).write_text(json.dumps(out, indent=1))


if __name__ == "__main__":
    main()
