"""Notebooks on disk, and notebooks as a typeset record.

A notebook is saved as Markdown that reads on its own -- the piece, its sets a
bar to a line, and every level's cells with whether each checks out -- followed
by an HTML comment holding the exact state as JSON. The readable part is for
people; the comment is what the app opens, so a saved notebook comes back
exactly as it was left.

The same notebook can be exported as LaTeX (and from it a PDF): the piece, then
level by level how it was concatenated, each expression typeset with € as the
paper writes it and, if asked, as Mathematica code for iC in SSSiCv102.wl.

Checks are made here, from the text of each cell, not taken from the browser:
what a saved file or an export says is ✓ is what expanding it actually gives.
"""
from __future__ import annotations

import colorsys
import json
import re
from datetime import datetime

import sympy

from . import encoders as E
from . import ic as IC

DATA_OPEN = "<!-- musicic:data v1"
DATA_CLOSE = "-->"
# The notebook's block hues (notebook.js HUES at 58% saturation, 40% lightness),
# so a € in an export is the colour it was on screen.
HUES = [200, 145, 275, 320, 175, 245, 95, 230, 290]
COLORS = ["".join(f"{round(c * 255):02X}" for c in colorsys.hls_to_rgb(h / 360, .40, .58))
          for h in HUES]


# --------------------------------------------------------------------------
# checking
# --------------------------------------------------------------------------

_SHOWN = {"dpitch": "Δpitch", "ioi": "gap"}


def columns(book: dict) -> list[tuple[str, str]]:
    """Each entry of a set, in the order the notebook writes them: its name and
    what its numbers mean."""
    key = (book.get("target") or {}).get("encoder") or book.get("encoder") or "pair"
    base = list(E.REGISTRY[key].columns) if key in E.REGISTRY else ["duration", "pitch"]
    order = book.get("order") or list(range(len(base)))
    grid = book.get("grid") or 32
    meaning = {
        "start": f"when the note begins, from the start of the piece; {grid} = a quarter note",
        "duration": f"how long the note lasts; {grid} = a quarter note",
        "pitch": "MIDI note number: 60 = middle C, 0 = a rest",
        "dpitch": "semitones up or down from the previous note; 1000 = a rest",
        "ioi": f"time until the next note begins; {grid} = a quarter note",
    }
    return [(_SHOWN.get(base[c], base[c]), meaning.get(base[c], "")) for c in order
            if c < len(base)]


def columns_text(book: dict) -> str:
    return "{" + ", ".join(n for n, _ in columns(book)) + "}"


def _row_text(r) -> str:
    return "{" + ",".join(str(x) for x in r) + "}"


def level_items(book: dict, k: int) -> list[dict]:
    """What level k works on: the piece's sets at level 1, afterwards the parts
    the level before was written as. Each is {text, rows}."""
    lv = book["levels"][k]
    if lv.get("items"):
        return lv["items"]
    target = book.get("target")
    if not target:
        return []
    return [{"text": _row_text(r), "rows": [i]} for i, r in enumerate(target["rows"])]


def evaluate(book: dict) -> list[dict]:
    """Expand every cell of every level and compare it with the piece.

    Per level: its items, and per cell the expansion's size, its parts, any
    error, and -- where the cell stands for some items -- whether it matches.
    """
    target = book.get("target")
    out = []
    for k, lv in enumerate(book.get("levels") or []):
        items = level_items(book, k)
        claims = list(lv.get("claims") or [])
        cells = []
        for n, text in enumerate(lv.get("cells") or []):
            claim = claims[n] if n < len(claims) else None
            rec = {"n": n + 1, "text": text, "claim": claim}
            if not text.strip():
                rec["empty"] = True
                cells.append(rec)
                continue
            try:
                tree = IC.parse(text)
                rows = IC.expand(tree)
                rec["tree"] = tree
                rec["rows"] = len(rows)
                rec["parts"] = len([p for p in IC.sources(text)["parts"] if p["rows"]])
                if target and claim:
                    want = [target["rows"][r] for it in claim for r in items[it]["rows"]]
                    rec["ok"] = [list(x) if isinstance(x, (list, tuple)) else x
                                 for x in rows] == want
            except Exception as exc:                  # noqa: BLE001 - reported
                rec["error"] = str(exc)
            cells.append(rec)
        used: dict = {}
        for c in cells:
            for i in c["claim"] or []:
                used[i] = used.get(i, 0) + 1
        ok_items = {i for c in cells if c.get("ok") for i in c["claim"]}
        done = bool(target) and bool(items) and len(ok_items) == len(items) \
            and all(v == 1 for v in used.values()) \
            and all(c.get("ok") for c in cells if c["claim"])
        out.append({"k": k, "items": items, "cells": cells, "written": len(ok_items),
                    "done": done,
                    "parts": sum(c.get("parts", 0) for c in cells if c["claim"])})
    return out


def _ranges(idx) -> str:
    a = sorted(set(idx))
    out, k = [], 0
    while k < len(a):
        j = k
        while j + 1 < len(a) and a[j + 1] == a[j] + 1:
            j += 1
        out.append(f"{a[k] + 1}–{a[j] + 1}" if j > k else f"{a[k] + 1}")
        k = j + 1
    return ", ".join(out)


def _by_bar(texts: list[str], bars: list) -> list[list[str]]:
    lines: list[list[str]] = []
    last = object()
    for t, b in zip(texts, bars):
        if not lines or b != last:
            lines.append([])
            last = b
        lines[-1].append(t)
    return lines


# --------------------------------------------------------------------------
# Markdown
# --------------------------------------------------------------------------

def to_markdown(book: dict) -> str:
    """The notebook as Markdown a person can read, and the app can reopen."""
    ev = evaluate(book)
    target = book.get("target")
    name = book.get("name") or "Untitled notebook"
    out = [f"# {name}", "",
           f"*musIC notebook · saved {datetime.now():%Y-%m-%d %H:%M}*", ""]
    bits = [f"{book.get('numerator', 4)}/{book.get('denominator', 4)}",
            f"{round(book.get('tempo') or 120)} bpm",
            f"each set is `{columns_text(book)}`"]
    if target:
        bits.insert(0, f"**{target.get('name') or name}**")
        bits.append(f"{len(target['rows'])} sets")
    out += [" · ".join(bits), ""]
    out += ["Each set is written " + columns_text(book) + ":", ""]
    out += [f"- **{n}** — {m}" for n, m in columns(book)] + [""]

    if target:
        out += ["## The piece's sets", "", "A bar to a line.", "", "```mathematica"]
        lines = _by_bar([_row_text(r) for r in target["rows"]], target.get("bars") or [])
        out.append("{" + ",\n ".join(",".join(l) for l in lines) + "}")
        out += ["```", ""]

    for lv in ev:
        noun = "sets" if lv["k"] == 0 else "items"
        head = f"## Level {lv['k'] + 1}"
        if target:
            head += (f" — {len(lv['items'])} {noun}"
                     + (f" → {lv['parts']} parts · done ✓" if lv["done"]
                        else f" · {lv['written']} of {len(lv['items'])} written"))
        out += [head, ""]
        if lv["k"] > 0:
            out += ["Items (level " + str(lv["k"]) + " as it was written):", "", "```mathematica"]
            out.append(", ".join(it["text"] for it in lv["items"]))
            out += ["```", ""]
        for c in lv["cells"]:
            if c.get("empty"):
                continue
            h = f"### In[{c['n']}]"
            if c["claim"]:
                h += f" — {noun} {_ranges(c['claim'])}"
                h += " · ✓ matches" if c.get("ok") else " · ✗ does not match" if "ok" in c else ""
            if c.get("error"):
                h += " · error"
            out += [h, "", "```mathematica", c["text"].rstrip(), "```", ""]
            if c.get("error"):
                out += [f"> {c['error']}", ""]

    state = {k: v for k, v in book.items() if k != "file"}
    data = json.dumps(state, ensure_ascii=False, separators=(",", ":"))
    # ">" is escaped so nothing in the data can close the comment early.
    data = data.replace(">", "\\u003e")
    out += ["---", "", "*The comment below is the exact notebook, for musIC to reopen.*", "",
            DATA_OPEN, data, DATA_CLOSE, ""]
    return "\n".join(out)


def from_markdown(text: str, name_hint: str = "") -> dict:
    """A notebook back from its Markdown. Without the data comment -- a file
    written by hand -- every ```mathematica block after the first heading
    becomes a cell of a single level."""
    m = re.search(re.escape(DATA_OPEN) + r"\s*(.*?)\s*" + re.escape(DATA_CLOSE), text, re.S)
    if m:
        book = json.loads(m.group(1))
        if not isinstance(book, dict) or "levels" not in book:
            raise ValueError("the data in this notebook file is not a musIC notebook")
        return book
    title = re.search(r"^#\s+(.+)$", text, re.M)
    cells = re.findall(r"```(?:mathematica|ic|wl)?\n(.*?)```", text, re.S)
    if not cells:
        raise ValueError("no musIC data and no code blocks to read as cells")
    return {"name": title.group(1).strip() if title else name_hint or "Notebook",
            "encoder": "pair", "numerator": 4, "denominator": 4, "tempo": 120, "grid": 32,
            "levels": [{"cells": [c.strip() for c in cells], "claims": []}], "level": 0}


def slug(name: str) -> str:
    s = re.sub(r"[^\w\s-]", "", name.lower()).strip()
    return re.sub(r"[\s_]+", "-", s)[:60] or "notebook"


# --------------------------------------------------------------------------
# LaTeX
# --------------------------------------------------------------------------

_TEX_ESC = {"\\": r"\textbackslash{}", "&": r"\&", "%": r"\%", "$": r"\$", "#": r"\#",
            "_": r"\_", "{": r"\{", "}": r"\}", "~": r"\textasciitilde{}",
            "^": r"\textasciicircum{}", "→": r"$\to$", "✓": r"\checkmark{}"}


def tex(s: str) -> str:
    return "".join(_TEX_ESC.get(ch, ch) for ch in str(s))


def tex_set(book: dict) -> str:
    """{pitch, duration} as LaTeX math, entry names in italics."""
    return r"\{" + ", ".join(rf"\mathit{{{tex(n)}}}" for n, _ in columns(book)) + r"\}"


def together(lv: dict, target: dict | None = None) -> dict | None:
    """A level's cells joined into one expression, in the order of the items
    they stand for: {tree, order, cells, rows, problems}. `order` gives each €
    the block number it has in its own cell, so the colours match the level's
    score. `problems` says why the whole is not the level -- cells left out,
    items not covered or covered twice, an expansion that differs from the
    piece -- and is empty only when it is. None when the level has fewer than
    two written cells: one cell is already the whole."""
    written = [c for c in lv["cells"] if not c.get("empty")]
    if len(written) < 2:
        return None
    noun = "sets" if lv["k"] == 0 else "items"
    problems = []
    for c in written:
        if c.get("error"):
            problems.append(f"In[{c['n']}] has an error, left out")
        elif not c["claim"]:
            problems.append(f"In[{c['n']}] stands for no {noun}, left out")
    cells = sorted((c for c in written if c["claim"] and "tree" in c),
                   key=lambda c: min(c["claim"]))
    tree: list = []
    order: dict = {}
    for c in cells:
        t = c["tree"]
        order.update(IC._blocks_order(t))
        # a cell written as a plain {...} of rows splices in; anything else is one entry
        if isinstance(t, list) and not isinstance(t, IC.Seq):
            tree.extend(t)
        else:
            tree.append(t)
    rows = IC.expand(tree)
    if target and lv["items"]:
        count: dict = {}
        for c in cells:
            for i in c["claim"]:
                count[i] = count.get(i, 0) + 1
        missing = [i for i in range(len(lv["items"])) if i not in count]
        twice = [i for i, v in count.items() if v > 1]
        if missing:
            problems.append(f"{noun} {_ranges(missing)} not covered")
        if twice:
            problems.append(f"{noun} {_ranges(twice)} written more than once")
        wrong = [c["n"] for c in cells if c.get("ok") is False]
        if wrong:
            problems.append(", ".join(f"In[{n}]" for n in wrong)
                            + " expand" + ("s" if len(wrong) == 1 else "")
                            + " to different notes from the piece")
        want = [target["rows"][r] for it in lv["items"] for r in it["rows"]]
        got = [list(x) if isinstance(x, (list, tuple)) else x for x in rows]
        if got != want and not problems:
            problems.append("expands to different notes from the piece")
    return {"tree": tree, "order": order, "cells": cells, "rows": rows,
            "problems": problems}


# --------------------------------------------------------------------------
# reasoning: what each € of a cell does, and what each of its steps gives
# --------------------------------------------------------------------------

STEPS_SHOWN = 12          # longer unfoldings show the first ones and the last


def _num(v) -> str:
    if hasattr(v, "denominator") and getattr(v, "denominator", 1) != 1:
        return f"{v.numerator}/{v.denominator}"
    if isinstance(v, float) and v.is_integer():
        return str(int(v))
    return str(v)


def _math(e) -> str:
    return IC._tex_leaf(e)


def _steps(node) -> list[tuple[str, dict]]:
    """Each repetition of a top-level €: its label and its index binding."""
    bs = IC._bindings(node.it, {})
    if node.it.is_count:
        return [(rf"copy {n + 1}", b) for n, b in enumerate(bs)]
    sym = sympy.Symbol(node.it.var)
    return [(rf"${node.it.var}={b[sym]}$", b) for b in bs]


def _column_words(name: str, leaf, node, steps) -> str:
    """How one entry of a set behaves over a €'s steps, in words."""
    nm = rf"\textit{{{tex(name)}}}"
    if not isinstance(leaf, sympy.Expr) or not leaf.free_symbols:
        return rf"{nm} stays {_num(IC._evaluate(leaf, {}))}"
    vals = [IC._evaluate(leaf, b) for _, b in steps]
    if node.it.is_count or len(set(map(str, vals))) == 1:
        return rf"{nm} stays {_num(vals[0])}" if vals else rf"{nm} is ${_math(leaf)}$"
    sym = sympy.Symbol(node.it.var)
    if leaf.free_symbols <= {sym} and len(vals) > 1:
        try:
            poly = sympy.Poly(leaf, sym)
        except sympy.PolynomialError:
            poly = None
        if poly is not None and poly.degree() == 1:
            d = sympy.nsimplify(poly.coeffs()[0])
            way = "rises" if d > 0 else "falls"
            return rf"{nm} {way} by {_num(abs(d))} each step, from {_num(vals[0])}"
    shown = ", ".join(_num(v) for v in vals[:6]) + (", …" if len(vals) > 6 else "")
    return rf"{nm} follows ${_math(leaf)}$: {shown}"


def _is_set(a) -> bool:
    return isinstance(a, list) and not isinstance(a, IC.Seq) and \
        not any(isinstance(x, (list, tuple, IC.IC)) for x in a)


def _depends(node, sym) -> bool:
    """Whether anything inside `node` uses the index `sym`."""
    if isinstance(node, IC.IC):
        lim = [node.it.start, node.it.stop]
        return any(isinstance(x, sympy.Expr) and sym in x.free_symbols for x in lim) \
            or any(_depends(a, sym) for a in node.args)
    if isinstance(node, (list, tuple)):
        return any(_depends(a, sym) for a in node)
    return isinstance(node, sympy.Expr) and sym in node.free_symbols


def _words(book: dict, node, steps) -> str:
    """A sentence on what one top-level € does."""
    n = len(steps)
    if node.it.is_count:
        head = f"Repeats {n} time{'s' if n != 1 else ''}"
    else:
        head = (rf"Runs ${node.it.var}$ from {_num(steps[0][1][sympy.Symbol(node.it.var)])} "
                rf"to {_num(steps[-1][1][sympy.Symbol(node.it.var)])} ({n} steps)"
                if steps else rf"Runs ${node.it.var}$ over an empty range, so writes nothing")
    if all(_is_set(a) for a in node.args):
        names = [nm for nm, _ in columns(book)]
        parts = []
        for k, a in enumerate(node.args):
            cols = "; ".join(_column_words(names[c] if c < len(names) else f"entry {c + 1}",
                                           leaf, node, steps) for c, leaf in enumerate(a))
            parts.append((f"set {k + 1}: " if len(node.args) > 1 else "") + cols)
        return head + ": " + "; ".join(parts) + "."
    m = len(node.args)
    head += f", writing {m} block{'s' if m != 1 else ''} each time"
    if node.it.is_count:
        return head + "; every copy is the same."
    sym = sympy.Symbol(node.it.var)
    moved = [k + 1 for k, a in enumerate(node.args) if _depends(a, sym)]
    if not moved:
        return head + "; every step is the same."
    return head + rf"; ${node.it.var}$ changes block{'s' if len(moved) != 1 else ''} " \
        + ", ".join(map(str, moved)) + "."


def reasoning_latex(book: dict, lv: dict, cell: dict) -> list[str]:
    """Why a cell is its items: a sentence per top-level €, then each of its
    steps and what that step writes -- the sets of the piece at level 1, the
    items of the level below (one step down) after that."""
    target = book.get("target")
    if not target or "tree" not in cell or not cell["claim"]:
        return []
    items = lv["items"]
    level1 = lv["k"] == 0
    # piece row and item of each row this cell expands to, in order
    want, item_at = [], []
    for i in cell["claim"]:
        for r in items[i]["rows"]:
            want.append(r)
            item_at.append(i)
    tree = cell["tree"]
    entries = [tree] if isinstance(tree, IC.IC) else list(tree)

    def gives(rows, off) -> str:
        n = len(rows)
        if not n:
            return "nothing"
        sets = [want[off + j] for j in range(n)] if off + n <= len(want) else []
        ok = sets and [list(x) if isinstance(x, (list, tuple)) else x for x in rows] \
            == [target["rows"][r] for r in sets]
        mark = r"\ \ok" if ok else r"\ \no"
        where = f"set{'s' if n != 1 else ''} {_ranges(sets)}" if sets else "past the end"
        if level1:
            vals = ", ".join(_row_text(r) for r in rows[:3]) + (", …" if n > 3 else "")
            return rf"{tex(vals)} = {where}{mark}"
        # one step down: the items of this level these rows are, when they
        # begin and end on an item's edges
        if sets and (off == 0 or item_at[off - 1] != item_at[off]) and \
                (off + n == len(item_at) or item_at[off + n] != item_at[off + n - 1]):
            its = sorted(set(item_at[off:off + n]))
            return rf"item{'s' if len(its) != 1 else ''} {tex(_ranges(its))} ({where}){mark}"
        return rf"{where}{mark}"

    L = [r"{\small\textit{Reasoning.}\par"]
    off = 0
    for e in entries:
        if isinstance(e, IC.IC):
            steps = _steps(e)
            L.append(_words(book, e, steps) + r"\par")
            rows_per = []
            for _, b in steps:
                rows: list = []
                for a in e.args:
                    IC._emit(rows, a, b)
                rows_per.append(rows)
            lines = []
            for k, ((label, _), rows) in enumerate(zip(steps, rows_per)):
                show = len(steps) <= STEPS_SHOWN or k < STEPS_SHOWN - 2 or k == len(steps) - 1
                if show:
                    lines.append(rf"\hspace{{1em}}{label} & $\to$ & {gives(rows, off)} \\")
                elif k == STEPS_SHOWN - 2:
                    lines.append(rf"\hspace{{1em}}\dots & & {len(steps) - STEPS_SHOWN + 1} "
                                 r"more steps \\")
                off += len(rows)
            if lines:
                L += [r"\begin{tabular}{@{}l@{\ }c@{\ }l@{}}"] + lines + [r"\end{tabular}\par"]
        else:
            rows: list = []
            IC._emit(rows, e, {})
            L.append(rf"{tex(IC.render(e))} is written out as it is: {gives(rows, off)}\par")
            off += len(rows)
    L.append(r"}\medskip")
    return L


def _together_latex(book: dict, lv: dict, noun: str, options: dict) -> list[str]:
    target = book.get("target")
    tg = together(lv, target)
    if not tg:
        return []
    head = rf"Level {lv['k'] + 1}, all together"
    if target:
        head += r"\quad " + (r"\no\ does not give the level" if tg["problems"]
                             else r"\ok\ matches")
    L = [r"\needspace{6\baselineskip}", rf"\subsection*{{{head}}}"]
    if tg["problems"]:
        L.append(r"{\color[HTML]{A8331E}" + "; ".join(tex(p) for p in tg["problems"])
                 + r"}\par")
    if not tg["cells"]:
        return L
    L.append(rf"{{\small\color{{black!60}}In[{', '.join(str(c['n']) for c in tg['cells'])}] "
             rf"joined in the order of the {noun} they stand for}}\par")
    if options.get("math"):
        L.append(r"{\raggedright\lineskiplimit=3pt\lineskip=7pt$\displaystyle "
                 + IC.render_latex(tg["tree"], COLORS, tg["order"]) + r"$\par}\medskip")
    if options.get("code"):
        L += [r"\begin{lstlisting}", IC.render_wl(tg["tree"]), r"\end{lstlisting}"]
    if options.get("items"):
        claimed = [i for c in tg["cells"] for i in c["claim"]]
        L.append(rf"{{\small\color{{black!60}}{noun} {tex(_ranges(claimed))} · "
                 rf"expands to {len(tg['rows'])} rows}}\par")
    return L


def to_latex(book: dict, options: dict, images: dict[str, str]) -> str:
    """A LaTeX record of the notebook, level by level.

    options: math, score, items, together, reasoning, code -- which parts
             to include.
    images:  file names of PNG scores already written beside the .tex, keyed
             "piece" for the plain score and "L1", "L2", ... for each level's
             score with that level's blocks coloured.
    """
    ev = evaluate(book)
    target = book.get("target")
    name = book.get("name") or "Untitled notebook"
    enc = book.get("encoder", "pair")
    L = [r"\documentclass[11pt]{article}",
         r"\usepackage[margin=2cm]{geometry}",
         r"\usepackage{fontspec}",
         r"\usepackage{amsmath,amssymb,eurosym,graphicx,xcolor,booktabs,listings}",
         r"\usepackage[hidelinks]{hyperref}",
         r"\usepackage{needspace}",
         r"\lstset{basicstyle=\ttfamily\small,breaklines=true,columns=fullflexible,"
         r"frame=single,framerule=0.3pt,rulecolor=\color{black!25},"
         r"backgroundcolor=\color{black!3},aboveskip=4pt,belowskip=8pt}",
         r"\setlength{\parindent}{0pt}\setlength{\parskip}{0.55em}",
         r"\newcommand{\ok}{\textcolor[HTML]{2F6B46}{\checkmark}}",
         r"\newcommand{\no}{\textcolor[HTML]{A8331E}{$\times$}}",
         r"\newcommand{\score}[1]{\begin{center}\includegraphics[width=\linewidth,"
         r"height=0.62\textheight,keepaspectratio]{#1}\end{center}}",
         r"\begin{document}",
         rf"{{\LARGE\bfseries {tex(name)}}}\par",
         r"{\large Indexed concatenation, level by level}\par",
         rf"{{\small\color{{black!60}} musIC · {datetime.now():%d %B %Y}}}\par\medskip"]

    meta = [f"{book.get('numerator', 4)}/{book.get('denominator', 4)}",
            f"{round(book.get('tempo') or 120)} bpm", rf"each set is ${tex_set(book)}$"]
    if target:
        meta.insert(0, rf"\textbf{{{tex(target.get('name') or name)}}}")
        meta.append(f"{len(target['rows'])} sets")
    L += [r"\section*{The piece}", " \\quad·\\quad ".join(meta) + r"\par"]
    L += [rf"Each set is written ${tex_set(book)}$:\par",
          r"{\small\begin{tabular}{@{\hspace{1em}}l@{\quad}l}"]
    L += [rf"$\mathit{{{tex(n)}}}$ & {tex(m)} \\" for n, m in columns(book)]
    L += [r"\end{tabular}}\par"]
    if options.get("score") and images.get("piece"):
        L.append(rf"\score{{{images['piece']}}}")
    if target and options.get("items"):
        L += [r"The piece as a set, a bar to a line:",
              r"\begin{lstlisting}",
              "{" + ",\n ".join(",".join(l) for l in _by_bar(
                  [_row_text(r) for r in target["rows"]], target.get("bars") or [])) + "}",
              r"\end{lstlisting}"]
    if target and options.get("items") and ev:
        L += [r"\subsection*{How far each level got}",
              r"\begin{tabular}{llll}\toprule",
              r"Level & works on & written as & \\\midrule"]
        for lv in ev:
            noun = "sets" if lv["k"] == 0 else "items"
            L.append(rf"{lv['k'] + 1} & {len(lv['items'])} {noun} & "
                     + (f"{lv['parts']} parts" if lv["done"] else "---") + " & "
                     + (r"\ok" if lv["done"] else f"{lv['written']} of {len(lv['items'])}")
                     + r" \\")
        L += [r"\bottomrule\end{tabular}"]

    for lv in ev:
        k, noun = lv["k"], ("sets" if lv["k"] == 0 else "items")
        title = f"Level {k + 1}"
        if target:
            title += (f": {len(lv['items'])} {noun} $\\to$ {lv['parts']} parts" if lv["done"]
                      else f": {lv['written']} of {len(lv['items'])} {noun} written")
        # keep a level's heading on the page with its score, not stranded above it
        L.append(r"\needspace{0.42\textheight}" if options.get("score") and images.get(f"L{k + 1}")
                 else r"\needspace{8\baselineskip}")
        L.append(rf"\section*{{{title}}}")
        L.append(rf"{{\small\color{{black!60}}sets are written ${tex_set(book)}$}}\par")
        if options.get("score") and images.get(f"L{k + 1}"):
            L.append(rf"\score{{{images[f'L{k + 1}']}}}")
        if k > 0 and options.get("items"):
            L += [rf"This level concatenates the {len(lv['items'])} parts level {k} was written as:",
                  r"\begin{lstlisting}", ", ".join(it["text"] for it in lv["items"]),
                  r"\end{lstlisting}"]
        for c in lv["cells"]:
            if c.get("empty"):
                continue
            head = rf"In[{c['n']}]"
            if c["claim"]:
                head += rf"\quad {noun} {tex(_ranges(c['claim']))}"
                if "ok" in c:
                    head += r"\quad " + (r"\ok\ matches" if c["ok"] else r"\no\ does not match")
            L.append(rf"\subsection*{{{head}}}")
            if c.get("error"):
                L += [r"\begin{lstlisting}", c["text"], r"\end{lstlisting}",
                      rf"\textcolor[HTML]{{A8331E}}{{{tex(c['error'])}}}"]
                continue
            if options.get("math"):
                # € carries its limits above and below; give wrapped lines room
                L.append(r"{\raggedright\lineskiplimit=3pt\lineskip=7pt$\displaystyle "
                         + IC.render_latex(c["tree"], COLORS) + r"$\par}\medskip")
            if options.get("code"):
                L += [r"\begin{lstlisting}", IC.render_wl(c["tree"]), r"\end{lstlisting}"]
            if options.get("items") and "rows" in c:
                L.append(rf"{{\small\color{{black!60}}expands to {c['rows']} rows"
                         + (f" · written as {c['parts']} part{'s' if c['parts'] != 1 else ''}"
                            if c.get("parts") else "")
                         + r"}\par")
            if options.get("reasoning") and lv["done"] and c.get("ok"):
                L += reasoning_latex(book, lv, c)
        if options.get("together") and (options.get("math") or options.get("code")):
            L += _together_latex(book, lv, noun, options)

    if target and ev:
        last = next((lv for lv in reversed(ev) if lv["done"]), None)
        if last:
            L += [r"\section*{Result}",
                  rf"Expanding level {last['k'] + 1} gives back all {len(target['rows'])} "
                  rf"sets of the piece \ok."]
    L.append(r"\end{document}")
    return "\n".join(L)
