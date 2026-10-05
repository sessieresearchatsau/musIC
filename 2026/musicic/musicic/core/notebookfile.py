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


def to_latex(book: dict, options: dict, images: dict[str, str]) -> str:
    """A LaTeX record of the notebook, level by level.

    options: math, score, items, code -- which parts to include.
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

    if target and ev:
        last = next((lv for lv in reversed(ev) if lv["done"]), None)
        if last:
            L += [r"\section*{Result}",
                  rf"Expanding level {last['k'] + 1} gives back all {len(target['rows'])} "
                  rf"sets of the piece \ok."]
    L.append(r"\end{document}")
    return "\n".join(L)
