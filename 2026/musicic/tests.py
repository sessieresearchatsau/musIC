"""Round-trip and agreement checks. Run: uv run python tests.py"""
from fractions import Fraction

from musicic.core import encoders as E
from musicic.core.ic import expand, expand_text, parse, spans
from musicic.core.metrics import measure
from musicic.core.midi_in import read_midi
from musicic.core.score import layout
from musicic.core.setfile import (SetFile, SetFileError, describe,
                                  from_noteset, parse_key)
from musicic.core.setfile import check as setfile_check
from musicic.core.setfmt import parse_set, to_mathematica

CORPUS = "/Users/derekrenck/Documents/GitHub/musIC/archive 2/"
FILES = ["happybirthdayinC.mid", "joy-to-the-world-flute.mid",
         "fur-elise.mid", "zelda-great-fairy-fountain.mid"]

fails = []


def check(name, got, want):
    if got != want:
        fails.append(f"{name}: got {got!r} want {want!r}")


# 1. Quantization matches the mappings written by hand in the notebooks.
NOTEBOOK = {455: 32, 341: 24, 113: 8, 911: 64, 227: 16, 683: 48, 56: 4, 27: 2}
for f in FILES:
    _, reports = read_midi(CORPUS + f)
    for r in reports:
        for s in r.duration_map:
            if s.raw in NOTEBOOK:
                check(f"{f} quantize {s.raw}", s.units, NOTEBOOK[s.raw])

# 2. Invertible encoders round-trip.
for f in FILES:
    sets, _ = read_midi(CORPUS + f)
    ns = max(sets, key=len)
    full = [(n.start, n.dur, n.pitch) for n in ns.sorted().notes]
    line = [(n.start, n.dur, n.pitch)
            for n in ns.monophonic_line()[0].notes if n.pitch]
    for key, meta in E.REGISTRY.items():
        if not meta.invertible:
            continue
        # Compare sounding notes; rests are covered by their own check below.
        back = [(n.start, n.dur, n.pitch)
                for n in E.decode(E.encode(ns, key), ns.grid) if n.pitch]
        want = full if key == "triple" else line
        if meta.caveat:                       # ioi drops articulation by design
            check(f"{f} {key}", [(s, p) for s, _, p in back],
                  [(s, p) for s, _, p in want])
        else:
            check(f"{f} {key}", back, want)

# 3. Every encoding row maps to a drawn note.
for f in FILES:
    sets, _ = read_midi(CORPUS + f)
    ns = max(sets, key=len)
    for key, meta in E.REGISTRY.items():
        if meta.nested:
            continue
        enc = E.encode(ns, key)
        drawn = {r for m in layout(ns, None, enc.source_notes)["measures"]
                 for n in m["notes"] for r in n["rows"]}
        missing = [i for i in range(len(enc.flat)) if i not in drawn]
        check(f"{f} {key} unmapped rows", missing, [])

# 4. IC expansion, including the paper's own examples.
for src, want in [
    ("IC3[{32,60}]", "{{32,60},{32,60},{32,60}}"),
    ("IC(i=0..3)[{32, 60+2*i}]", "{{32,60},{32,62},{32,64},{32,66}}"),
    ("IC(i=0..1)[{62, 24/3^i}]", "{{62,24},{62,8}}"),
    ("IC(j=0..3)[{1/6*(j^3-13*j)+81}]", "{{81},{79},{78},{79}}"),
    ("{{1,1}, IC0[{9,9}], {2,2}}", "{{1,1},{2,2}}"),
    ("{IC5[[1]], IC4[[2]]}", "{1,1,1,1,1,2,2,2,2}"),
    # Paper eq. 2: the first rows of the infinite network's reduced EDSL.
    ("IC(k=1..2)[{1,1,2,2}, IC(j=1..2)[{2,2*j}], {1,2*k+3}]",
     "{{1,1,2,2},{2,2},{2,4},{1,5},{1,1,2,2},{2,2},{2,4},{1,7}}"),
]:
    check(f"IC {src}", to_mathematica(expand_text(src)), want)

# 4b. Spans: every expanded row is covered, and repeats are all reported.
for src, rows_want, span_want in [
    ("IC4[IC(i=1..3)[{8, 60+i}], {16, 67}]", 16, 5),
    ("IC3[IC(i=1..3)[{8, 60+i}], IC2[{16, 72}], {32, 67}]", 18, 7),
    ("IC49[IC3[{8,-1}], {8,1}]", 196, 50),
]:
    tree = parse(src)
    check(f"spans rows {src[:26]}", len(expand(tree)), rows_want)
    sp = spans(tree)
    check(f"spans count {src[:26]}", len(sp), span_want)
    outer = [s for s in sp if s["depth"] == 0]
    check(f"spans cover {src[:26]}", outer[0]["end"], rows_want - 1)
    for s in sp:                       # never point past the expansion
        check(f"spans bound {src[:26]}", s["end"] < rows_want, True)

# 5. Set literals in every dialect we accept.
for src, want in [
    ("{{0,455,60},{1/2,455,60}}", "{{0,455,60},{1/2,455,60}}"),
    ("{{{0,113,81},{3/16,113,74}}}", "{{0,113,81},{3/16,113,74}}"),
    ('{StyleBox[{62,24}, Background->RGBColor[0.5,0.,0.008]],{62,8}}', "{{62,24},{62,8}}"),
    ('RowBox[{"{", RowBox[{"{", RowBox[{FractionBox["11","8"], ",", "62"}], "}"}], "}"}]',
     "{{11/8,62}}"),
    ("[[32, 60], [24, 62]]", "{{32,60},{24,62}}"),
]:
    check(f"parse {src[:34]}", to_mathematica(parse_set(src)), want)

# 5b. Rests survive a round trip wherever they sit, including at the ends.
from musicic.core.notes import NoteSet, REST                      # noqa: E402

for rows in ([(16, 60), (16, REST), (32, 67), (8, REST)],
             [(8, REST), (16, 60), (32, 67)],
             [(16, REST)] * 3,
             [(16, 60), (16, 67)]):
    enc = E.Encoding(rows, ("duration", "pitch"), "pair",
                     anchor={"first_pitch": 60})
    ns = NoteSet(notes=E.decode(enc, 32), grid=32)
    check(f"rest round-trip {rows[0]}",
          [tuple(r) for r in E.encode(ns, "pair").flat], rows)

# 5c. Notation: key spelling is sound, accidentals are not repeated, and long
# pieces page instead of rendering all at once.
from musicic.core.keysig import LETTER_PC, BarAccidentals, Key   # noqa: E402
from musicic.core.score import layout as _layout                  # noqa: E402

for root, minor in [(0, False), (5, False), (6, False), (1, False), (9, True)]:
    k = Key(root, minor)
    for pitch in range(36, 96):
        letter, alter, _ = k.spell(pitch)
        check(f"spell {k.display} {pitch}", (LETTER_PC[letter] + alter) % 12,
              pitch % 12)

k = Key(0)
ba = BarAccidentals(k)
letter, alter, octave = k.spell(66)                 # F#
check("accidental first time", ba.needed(letter, alter, octave), "#")
check("accidental repeated", ba.needed(letter, alter, octave), None)
letter, alter, octave = k.spell(65)                 # F natural, same bar
check("accidental cancels", ba.needed(letter, alter, octave), "n")
ba.reset()
letter, alter, octave = k.spell(66)
check("accidental after barline", ba.needed(letter, alter, octave), "#")

for f in FILES:
    sets, _ = read_midi(CORPUS + f)
    ns = max(sets, key=len).top_line()
    enc = E.encode(ns, "pair")
    first = _layout(ns, 8, enc.source_notes, page=0)
    check(f"{f} page size", len(first["measures"]) <= 8, True)
    if first["pages"] > 1:                          # later pages hold real bars
        last = _layout(ns, 8, enc.source_notes, page=first["pages"] - 1)
        check(f"{f} last page", len(last["measures"]) > 0, True)
        check(f"{f} pages advance",
              last["measures"][0]["index"] > first["measures"][0]["index"], True)
    # Every drawn note carries an accidental slot per key.
    for m in first["measures"]:
        for n in m["notes"]:
            if not n["rest"]:
                check(f"{f} acc slots", len(n["accidentals"]), len(n["keys"]))

# 6. Metrics behave on constructed cases.
check("metrics constant", measure([(1, 2)] * 8).runs, 1)
check("metrics period", measure([(1, 2), (3, 4)] * 6).best_period, 2)
check("metrics distinct", measure([(1,), (2,), (3,)]).distinct, 3)

# 7. The reducer is parked, not wired into the app. This only guards the one
# property that matters if it is ever picked back up: it must never return
# something that fails to expand back to its input.
from musicic.core.reduce import reduce_rows          # noqa: E402

for rows in ([(32, 60)] * 8,
             [(16, 60 + 2 * i) for i in range(8)],
             [(32, 60), (16, 67), (24, 71), (8, 62)],
             [(8, 61), (8, 62), (8, 63), (16, 67)] * 4):
    r = reduce_rows(rows)
    check(f"reduce lossless {len(rows)}", r.verified, True)
    check(f"reduce expands {len(rows)}",
          [tuple(x) for x in expand(r.tree)], [tuple(x) for x in rows])

# 8. Set files: the path a transcribed picture takes into the app.
SCORES = "/Users/derekrenck/Documents/GitHub/musIC/2026/musicic/scores/"

# Every shipped transcription parses, checks clean and draws.
for name in ("happy-birthday-melody.music.json",
             "happy-birthday-piano.music.json"):
    sf = SetFile.load(SCORES + name)
    check(f"{name} warnings", [w.message for w in setfile_check(sf)], [])
    ns = sf.to_noteset()
    bar = sf.numerator * 4 * sf.grid // sf.denominator
    check(f"{name} whole bars", ns.span % bar, 0)
    check(f"{name} describes", "nothing suspicious" in describe(sf), True)
    check(f"{name} draws", len(layout(ns, 8)["measures"]) > 0, True)

# The two readings of the same picture must agree: the top line of the piano
# transcription is exactly the melody transcription.
mel = SetFile.load(SCORES + "happy-birthday-melody.music.json").to_noteset()
pno = SetFile.load(SCORES + "happy-birthday-piano.music.json").to_noteset()
check("picture readings agree",
      [(n.start, n.dur, n.pitch) for n in pno.top_line().sorted().notes],
      [(n.start, n.dur, n.pitch) for n in mel.sounding().sorted().notes])

# A file survives a write/read cycle unchanged, so Save .set is lossless.
sf = SetFile.load(SCORES + "happy-birthday-melody.music.json")
check("setfile dumps round-trip", SetFile.load_text(sf.dumps()).rows, sf.rows)
check("setfile from_noteset round-trip",
      from_noteset(sf.to_noteset(), "pair").rows, sf.rows)

# `set` accepts a literal string and a JSON array alike.
LIT = "{{32,60},{16,62}}"
check("setfile literal", SetFile.load_text(LIT).rows, [(32, 60), (16, 62)])
check("setfile literal in json",
      SetFile.load_text('{"set": "' + LIT + '"}').rows, [(32, 60), (16, 62)])
check("setfile json array", SetFile.load_text('{"set": [[32,60],[16,62]]}').rows,
      [(32, 60), (16, 62)])

# Key names both ways.
for text, want in [("F", (5, False)), ("Bb", (10, False)), ("Dm", (2, True)),
                   ("C# minor", (1, True)), ("G major", (7, False)),
                   (None, (None, False))]:
    check(f"parse_key {text}", parse_key(text), want)

# Malformed files are refused with a message, not a traceback -- the UI shows
# whatever SetFileError says, so anything else reaches the user as a 500.
def rejects(doc, why):
    try:
        sf = SetFile.load_text(doc)
        setfile_check(sf)                          # key names are checked here
    except SetFileError:
        return
    except Exception as exc:                       # noqa: BLE001
        fails.append(f"reject {why}: raised {type(exc).__name__}, not SetFileError")
        return
    fails.append(f"reject {why}: accepted {doc}")


rejects('{"set": []}', "empty")
rejects('{"set": [[32,60,9]]}', "wrong width")
rejects('{"set": [[32,60],[32]]}', "ragged")
rejects('{"set": [[32,60]], "encoder": "contour"}', "lossy encoder")
rejects('{"set": [[32,60]], "encoder": "nope"}', "unknown encoder")
rejects('{"set": [[32.5,60]]}', "fractional duration")
rejects('{"set": [[32,60]], "grid": 0}', "grid 0")
rejects('{"set": [[32,60]], "key": "H"}', "bad key name")
rejects('{"musicic": 99, "set": [[32,60]]}', "future version")
rejects('{"set": [[[32,60]]]}', "nested")
rejects('{"name": "no rows"}', "no set key")

# The checks fire on the mistakes they exist for, and stay quiet otherwise.
def kinds(doc):
    return sorted({w.kind for w in setfile_check(SetFile.load_text(doc))})

check("check clean", kinds('{"set": [[32,60],[32,62],[32,64],[32,65]]}'), [])
check("check off-keyboard", "pitch" in kinds('{"set": [[128,200]]}'), True)
check("check short bar", "barline" in kinds('{"set": [[32,60]]}'), True)

print("\n".join(fails) if fails else "all checks pass")
raise SystemExit(1 if fails else 0)
