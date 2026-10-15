"""Every approach of musicic/core/strategies.py on every set we have: the
Happy Birthday transcriptions and the MIDI the old experiments were built from
(through several encoders, with their bar lines), the reference sets of
docs/concatenation-examples.md, the networks of the ICN paper and the stress
forms of docs/networks.

Run: uv run python compare_strategies.py [--limit SECONDS] [--only TEXT] [--jobs N]

--only runs the sets whose names contain TEXT (several, comma-separated) and
keeps the others' results.

Each run gets the time limit in a process of its own, several at once. The
results go to docs/strategies/results.json, which the Concatenate page shows,
and a table is printed: rows written, then numbers written, for each set and
approach. A result that does not expand back exactly is marked LOSSY; none
should be."""
import json
import multiprocessing as mp
import re
import sys
import time
from dataclasses import replace
from pathlib import Path

HERE = Path(__file__).parent
sys.path.insert(0, str(HERE / "docs" / "networks"))

from paper_forms import FORMS, rows as paper_rows     # noqa: E402
from stress_forms import STRESS                       # noqa: E402

from musicic.core import encoders as E                # noqa: E402
from musicic.core.ic import expand, parse             # noqa: E402
from musicic.core.midi_in import read_midi            # noqa: E402
from musicic.core.notes import Note, NoteSet          # noqa: E402
from musicic.core.setfile import SetFile              # noqa: E402
from musicic.core.strategies import STRATEGIES, bars_of, run  # noqa: E402

ARCHIVE = HERE.parent.parent / "archive 2"
MIDIS = HERE.parent / "midis"
# name -> (file, track, adjustments). track: an index, "all" (every track in
# one set), or None (the longest). scale multiplies every time (a file written
# in sixteenths for quarters); meter overrides the time signature read, which
# takes the last one in the file; bars keeps the first so many bars.
MIDI = {"hbd-in-C": (ARCHIVE / "happybirthdayinC.mid", None, {}),
        "joy": (ARCHIVE / "joy-to-the-world-flute.mid", None, {}),
        "fur-elise": (ARCHIVE / "fur-elise.mid", None, {}),
        "zelda": (ARCHIVE / "zelda-great-fairy-fountain.mid", None, {}),
        "feliz-navidad": (ARCHIVE / "Feliz_Navidad_easy_piano.mid", None, {}),
        "experiment1": (ARCHIVE / "experiment1.mid", None, {}),
        "mario": (MIDIS / "supermariobrostheme.mid", None, {}),
        "frere-jacques-midi": (MIDIS / "frere-jacques.mid", 0, {"scale": 4}),
        "bach-bwv846-upper": (MIDIS / "bach-bwv846.mid", 1, {}),
        "bach-bwv846": (MIDIS / "bach-bwv846.mid", "all", {}),
        "canon": (MIDIS / "pachelbel-canon-in-d.mid", 0, {"bars": 24}),
        "blues-bass": (MIDIS / "twelve-bar-blues.mid", 1, {}),
        "blues-riff": (MIDIS / "twelve-bar-blues.mid", 2, {}),
        "mozart-theme": (MIDIS / "mozart-k265.mid", 0, {"meter": (2, 4), "bars": 48}),
        "mozart-theme-var1-2": (MIDIS / "mozart-k265.mid", 0, {"meter": (2, 4), "bars": 144})}
ENCODERS = ["pair", "triple", "interval", "ioi", "degree", "contour"]
OUT = HERE / "docs" / "strategies" / "results.json"


def music_sets() -> dict:
    out = {}
    pieces = [(p.name.split(".")[0], SetFile.load(p).to_noteset())
              for p in sorted((HERE / "scores").glob("*.music.json"))]
    for name, (path, track, adj) in MIDI.items():
        if path.exists():
            pieces.append((name, midi_piece(path, track, adj)))
    for name, ns in pieces:
        for key in ENCODERS:
            enc = E.encode(ns, key)
            if enc.nested:
                continue
            rows = [tuple(int(x) for x in r) for r in enc.flat]
            out[f"{name}:{key}"] = {"group": "music", "rows": rows, "bars": bars_of(enc, ns)}
    return out


def midi_piece(path, track, adj: dict) -> NoteSet:
    sets, _ = read_midi(str(path))
    if track == "all":
        ns = replace(sets[0], notes=sorted((n for s in sets for n in s.notes),
                                             key=lambda n: (n.start, n.pitch)))
    else:
        ns = max(sets, key=len) if track is None else sets[track]
    k = adj.get("scale", 1)
    notes = [Note(n.start * k, n.dur * k, n.pitch) for n in ns.notes]
    num, den = adj.get("meter", (ns.numerator, ns.denominator))
    if "bars" in adj:
        end = adj["bars"] * num * 4 * ns.grid // den
        notes = [n for n in notes if n.start < end]
    return replace(ns, notes=notes, numerator=num, denominator=den)


def sets() -> dict:
    out = music_sets()
    doc = (HERE / "docs" / "concatenation-examples.md").read_text()
    for n in range(1, 7):
        block = doc.split(f"### {n}. Set {n}")[1].split("```")[1]
        rows = [tuple(int(x) for x in re.findall(r"-?\d+", m))
                for m in re.findall(r"\{([^{}]*)\}", block)]
        out[f"ref{n}"] = {"group": "reference", "rows": rows, "bars": None}
    for name in FORMS:
        out[name] = {"group": "network", "rows": paper_rows(name), "bars": None}
    for name, text in STRESS.items():
        out[f"stress:{name}"] = {"group": "stress", "bars": None,
                                 "rows": [tuple(x) for x in expand(parse(text))]}
    return out


def _work(code, rows, bars, q):
    try:
        o = run(code, rows, bars)
        q.put({"text": o.text, "lossless": o.verified, "seconds": o.seconds, **o.measures})
    except Exception as exc:                       # noqa: BLE001
        q.put({"error": f"{type(exc).__name__}: {exc}"})


def main():
    args = sys.argv[1:]
    limit = float(args[args.index("--limit") + 1]) if "--limit" in args else 120
    only = args[args.index("--only") + 1].split(",") if "--only" in args else None
    jobs = int(args[args.index("--jobs") + 1]) if "--jobs" in args else max(1, mp.cpu_count() - 2)
    todo, results = [], {}
    for name, s in sets().items():
        if only and not any(o in name for o in only):
            continue
        results[name] = {"group": s["group"], "n": len(s["rows"]),
                         "bars": s["bars"] is not None, "runs": {}}
        for st in STRATEGIES:
            if st.needs_bars and s["bars"] is None:
                results[name]["runs"][st.code] = {"skipped": "no bar lines"}
                continue
            todo.append((name, st.code, s["rows"], s["bars"]))

    running: list = []
    t_all = time.time()
    while todo or running:
        while todo and len(running) < jobs:
            name, code, rows, bars = todo.pop(0)
            q = mp.Queue()
            p = mp.Process(target=_work, args=(code, rows, bars, q))
            p.start()
            running.append((name, code, p, q, time.time()))
        time.sleep(0.05)
        for item in list(running):
            name, code, p, q, t0 = item
            if not q.empty():
                results[name]["runs"][code] = q.get()
                p.join()
            elif not p.is_alive():
                results[name]["runs"][code] = {"crash": True}
            elif time.time() - t0 > limit:
                p.terminate()
                results[name]["runs"][code] = {"timeout": limit}
            else:
                continue
            running.remove(item)

    OUT.parent.mkdir(parents=True, exist_ok=True)
    if only and OUT.exists():
        # a partial run updates its sets and keeps the rest
        kept = json.loads(OUT.read_text())["sets"]
        kept.update(results)
        results = {**kept}
    OUT.write_text(json.dumps({"limit": limit, "strategies": [s.code for s in STRATEGIES],
                               "sets": results}, indent=1))
    table(results)
    print(f"\n{len(results)} sets in {time.time() - t_all:.0f} s -> {OUT.relative_to(HERE)}")


def cell(d) -> str:
    if "skipped" in d:
        return "—"
    if "timeout" in d:
        return "timeout"
    if "error" in d or "crash" in d:
        return "error"
    return f"{d['rows']}/{d['numbers']}" + ("" if d["lossless"] else " LOSSY")


def table(results):
    codes = [s.code for s in STRATEGIES]
    print(f"{'set':28}{'n':>6}  " + "".join(f"{c:>13}" for c in codes))
    for name, r in results.items():
        print(f"{name:28}{r['n']:>6}  " + "".join(f"{cell(r['runs'].get(c, {})):>13}" for c in codes))
    print("\ncells are rows/numbers written; — needs bar lines the set does not have")


if __name__ == "__main__":
    main()
