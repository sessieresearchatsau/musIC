"""concat.py on the networks of the ICN paper (docs/networks): every result must
expand back to the network's rows, and is measured against the paper's form.

Run: uv run python network_tests.py [name ...] [--save results.json]

Two sizes are reported for each form, ours and the paper's:
  rows     rows written down (concat.leaves): a row is 1, an IC its body
  numbers  numbers and index names written, iterators included: how much has
           to be read to state the expression
A set passes when the result is lossless and no larger than the paper's,
rows first and then numbers, as concat.py itself decides: "ok" when neither
size is larger, "rows-" when it has fewer rows but more numbers. Each set gets
TIME_LIMIT seconds."""
import json
import multiprocessing as mp
import re
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent / "docs" / "networks"))

from paper_forms import FORMS, rows             # noqa: E402

from musicic.core.concat import concatenate, leaves  # noqa: E402
from musicic.core.ic import expand, parse, render     # noqa: E402

TIME_LIMIT = 60


def numbers(tree) -> int:
    return len(re.findall(r"\d+|[A-Za-z]\w*", render(tree)))


def _run(name, q):
    t = time.time()
    r = concatenate(rows(name))
    ok = [tuple(x) for x in expand(r.tree)] == rows(name)
    q.put({"text": r.text, "rows": r.size, "numbers": numbers(r.tree),
           "lossless": ok and r.verified, "seconds": round(time.time() - t, 2)})


def run(name) -> dict:
    q = mp.Queue()
    p = mp.Process(target=_run, args=(name, q))
    p.start()
    p.join(TIME_LIMIT)
    if p.is_alive():
        p.terminate()
        return {"text": None, "lossless": False, "seconds": TIME_LIMIT, "timeout": True}
    return q.get() if not q.empty() else {"text": None, "lossless": False, "crash": True}


def main():
    args = sys.argv[1:]
    save = None
    if "--save" in args:
        save = args[args.index("--save") + 1]
        args = args[:args.index("--save")]
    names = args or list(FORMS)
    out, fails = {}, 0
    for name in names:
        where, text = FORMS[name]
        paper = parse(text)
        res = run(name)
        res.update(paper_rows=leaves(paper), paper_numbers=numbers(paper),
                   n=len(rows(name)))
        if res.get("text") is None:
            verdict = "TIMEOUT" if res.get("timeout") else "CRASH"
        elif not res["lossless"]:
            verdict = "LOSSY"
        elif res["rows"] <= res["paper_rows"] and res["numbers"] <= res["paper_numbers"]:
            verdict = "ok"
        elif res["rows"] < res["paper_rows"]:
            verdict = "rows-"           # fewer rows, more numbers: still smaller
        else:
            verdict = "larger"
        res["verdict"] = verdict
        fails += verdict not in ("ok", "rows-")
        out[name] = res
        print(f"{name:6} {verdict:7} rows {res.get('rows', '-')!s:>4}/{res['paper_rows']:<3}"
              f" numbers {res.get('numbers', '-')!s:>4}/{res['paper_numbers']:<4}"
              f" {res['seconds']:>6}s  n={res['n']}")
        if res.get("text"):
            print(f"       {res['text'][:400]}")
    print(f"\n{len(names) - fails}/{len(names)} at least as compact as the paper")
    if save:
        Path(save).write_text(json.dumps(out, indent=1))


if __name__ == "__main__":
    main()
