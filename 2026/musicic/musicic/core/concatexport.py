"""The Concatenate page as LaTeX, for Export PDF: a set, how one approach
concatenated it, and, when every approach was run, how they compare.

Nothing here assumes the set is music: a set is rows of integers, and an
approach is one of core/strategies.py. Each expression is parsed back from
the text the page shows and expanded again before it is typeset, so the PDF
says only what it has checked.
"""
from __future__ import annotations

from datetime import datetime

from . import ic as IC
from .notebookfile import COLORS, tex
from .strategies import BY_CODE

SET_ROWS_SHOWN = 400      # a longer set prints its first and last rows
MATH_PARTS = 400          # a longer result is given as code only


def _row(r) -> str:
    return "{" + ", ".join(str(x) for x in r) + "}"


def _tree(text: str):
    t = IC.parse(text)
    return t if isinstance(t, list) else [t]


def _expands(tree, rows) -> bool:
    try:
        return [tuple(x) for x in IC.expand(list(tree))] == [tuple(r) for r in rows]
    except Exception:                              # noqa: BLE001
        return False


def _math(tree) -> str:
    """Display math that wraps: € carries its limits above and below, so
    wrapped lines get room."""
    return (r"{\raggedright\lineskiplimit=3pt\lineskip=7pt$\displaystyle "
            + IC.render_latex(tree, COLORS) + r"$\par}\medskip")


def _code(text: str) -> list[str]:
    """A code box. listings breaks a line after any € it meets, so € is
    written IC there, the spelling musIC reads as the same sign."""
    return [r"\begin{lstlisting}", text.replace("€", "IC"), r"\end{lstlisting}"]


def _measures(m: dict, seconds, verified: bool) -> list[str]:
    """A two-column table; the values are LaTeX already."""
    tools = " ".join(m.get("tools") or []) or "E1"
    cells = [("rows written", m.get("rows")), ("numbers written", m.get("numbers")),
             ("parts at the top", m.get("parts")), ("nesting depth", m.get("depth")),
             ("formulas in rows", m.get("formulas")), ("€ tools used", tex(tools))]
    if m.get("bar_fit") is not None:
        cells.append(("bar fit", rf"{round(m['bar_fit'] * 100)}\%"))
    cells += [("time", f"{seconds} s"),
              ("expands back exactly", r"\ok" if verified else r"\no")]
    return ([r"\begin{tabular}{ll}\toprule"]
            + [rf"{tex(a)} & {b} \\" for a, b in cells]
            + [r"\bottomrule\end{tabular}\par"])


def _approach(code: str) -> list[str]:
    st = BY_CODE.get(code)
    if st is None:
        return []
    choose = {"G": "greedy-shortest", "S": "best-saving", "B": "bar-anchored",
              "V": "variant-merging"}[st.choose]
    build = {"F": "flat, then nested", "U": "up, level by level"}[st.build]
    tools = ", ".join(["E1"] + st.tools.split()) if st.tools else "E1"
    return [rf"{tex(st.blurb)}\par",
            rf"{{\small\color{{black!60}}blocks chosen: {choose} · levels: {build} · "
            rf"€ may use: {tex(tools)}}}\par"]


def _structure(tree) -> list[str]:
    out = [r"\begin{tabular}{rrl}\toprule", r"rows & count & kind \\\midrule"]
    at = 0
    for item in tree:
        n = len(IC.expand([item]))
        kind = ("repeat" if item.it.is_count else "indexed block") if isinstance(item, IC.IC) else "row"
        out.append(rf"{at + 1}{'' if n == 1 else f'--{at + n}'} & {n} & {kind} \\")
        at += n
    out.append(r"\bottomrule\end{tabular}\par")
    return out


def _result(res: dict, rows, options: dict, heading: str) -> list[str]:
    """One approach's answer: what it is, then the parts asked for."""
    tree = _tree(res["text"])
    ok = _expands(tree, rows)
    L = [rf"\section*{{{heading}}}"]
    if options.get("approach"):
        L += _approach(res.get("code", ""))
    if options.get("measures") and res.get("measures"):
        L += [r"\subsection*{Measures}"] + _measures(res["measures"], res.get("seconds", "?"), ok)
    if options.get("math"):
        L.append(r"\subsection*{The concatenation}")
        if len(tree) <= MATH_PARTS:
            L.append(_math(tree))
        else:
            L.append(rf"{len(tree)} parts at the top: too long to typeset as math"
                     + (r", so it is given as Mathematica code below.\par" if options.get("code")
                        else r".\par"))
    if options.get("code"):
        L += [r"\subsection*{Mathematica}", r"{\small iC from SSSiCv102.wl; "
              r"\texttt{ExpandAll} gives the set back.}\par"] + _code(IC.render_wl(tree))
    if options.get("structure"):
        L += [r"\subsection*{Structure}"] + _structure(tree)
    if options.get("levels") and res.get("levels"):
        L += [r"\subsection*{Levels}",
              r"{\small\color{black!60}IC is the € sign, written as musIC reads it.}\par"]
        lv = res["levels"]
        for k, parts in enumerate(lv):
            src = f"{len(rows)} rows" if k == 0 else f"{len(lv[k - 1])} parts"
            L.append(rf"\paragraph{{Level {k + 1}}} {src} $\to$ {len(parts)} "
                     + ("pieces" if k == 0 else "parts") + r"\par")
            L += _code(",\n".join(p["text"] for p in parts))
    return L


def to_latex(name: str, rows: list, result: dict | None, compared: list | None,
             options: dict) -> str:
    """options: set, approach, math, code, measures, structure, levels,
    compare -- which parts to include."""
    rows = [tuple(int(x) for x in r) for r in rows]
    width = sorted({len(r) for r in rows})
    L = [r"\documentclass[11pt]{article}",
         r"\usepackage[margin=2cm]{geometry}",
         r"\usepackage{fontspec}",
         r"\usepackage{amsmath,amssymb,eurosym,xcolor,booktabs,listings,longtable}",
         r"\usepackage[hidelinks]{hyperref}",
         r"\usepackage{needspace}",
         r"\lstset{basicstyle=\ttfamily\small,breaklines=true,columns=fullflexible,"
         r"frame=single,framerule=0.3pt,rulecolor=\color{black!25},"
         r"backgroundcolor=\color{black!3},aboveskip=4pt,belowskip=8pt}",
         r"\setlength{\parindent}{0pt}\setlength{\parskip}{0.55em}",
         r"\newcommand{\ok}{\textcolor[HTML]{2F6B46}{\checkmark}}",
         r"\newcommand{\no}{\textcolor[HTML]{A8331E}{$\times$}}",
         r"\begin{document}",
         rf"{{\LARGE\bfseries {tex(name)}}}\par",
         r"{\large Indexed concatenation}\par",
         rf"{{\small\color{{black!60}} musIC · {datetime.now():%d %B %Y}}}\par\medskip"]

    shape = (f"{width[0]} numbers each" if len(width) == 1
             else f"{width[0]} to {width[-1]} numbers each")
    L += [r"\section*{The set}", rf"{len(rows)} rows, {tex(shape)}.\par"]
    if options.get("set"):
        if len(rows) <= SET_ROWS_SHOWN:
            L += _code("{" + ",\n ".join(", ".join(_row(r) for r in rows[i:i + 8])
                                          for i in range(0, len(rows), 8)) + "}")
        else:
            half = SET_ROWS_SHOWN // 2
            L += [rf"The first and last {half} of its {len(rows)} rows:\par"]
            L += _code(", ".join(_row(r) for r in rows[:half]) + ",\n…,\n"
                       + ", ".join(_row(r) for r in rows[-half:]))

    if options.get("compare") and compared:
        done = [c for c in compared if not c.get("skipped")]
        L += [r"\section*{The approaches compared}",
              r"Every approach on this set. Rows and numbers are what is written down "
              r"to state the concatenation; fewer rows, then fewer numbers, is smaller.\par",
              r"\begin{tabular}{llrrrrl}\toprule",
              r"approach & name & rows & numbers & parts & depth & € tools \\\midrule"]
        for c in compared:
            if c.get("skipped"):
                L.append(rf"{tex(c['code'])} & {tex(c['name'])} & \multicolumn{{5}}{{l}}"
                         rf"{{--- {tex(c['skipped'])}}} \\")
                continue
            m = c["measures"]
            tools = " ".join(t for t in m["tools"] if t != "E1") or "E1"
            L.append(rf"{tex(c['code'])} & {tex(c['name'])} & {m['rows']} & {m['numbers']} & "
                     rf"{m['parts']} & {m['depth']} & {tex(tools)} \\")
        L.append(r"\bottomrule\end{tabular}\par")
        for c in done:
            L += _result(c, rows, options, f"{c['code']} --- {c['name']}")
    elif result:
        L += _result(result, rows, options, f"{result.get('code', '')} --- {result.get('name', '')}")

    L.append(r"\end{document}")
    return "\n".join(L)
