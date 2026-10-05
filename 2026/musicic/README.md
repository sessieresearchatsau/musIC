# musIC — music ⇄ set workbench

A local tool for the Indexed Concatenation music work: turn scores into sets
many different ways, see and hear any set as notation, and measure which
encoding is worth reducing.

```bash
cd 2026/musicic
uv sync
uv run musicic          # serves http://127.0.0.1:8730 and opens a browser
```

`uv run musicic --port 9000 --no-browser` if you want it elsewhere.

## What it does

**MIDI → set.** Reads a `.mid`, pairs note-ons with note-offs, and quantizes raw
ticks onto the musical grid — automating the replacement rules the notebooks
write by hand. It reproduces those mappings exactly: `455→32`, `341→24`,
`113→8`, `911→64`, `227→16`, `683→48`, `56→4`, `27→2`. The derived table is
shown with the distance each value travelled, and anything that moved unusually
far is flagged rather than silently accepted.

**Picture → set → score.** The tool does not read images: OMR on a photograph
is unreliable in exactly the way that matters when every integer has to be
right. The reading is done by eye and written as a **set file** — a set literal
plus the bar, grid, tempo and key a bare set cannot state about itself:

```json
{ "name": "Happy Birthday to You — melody", "encoder": "pair",
  "grid": 32, "tempo": 120, "numerator": 3, "denominator": 4, "key": "C",
  "set": [[16, 67], [16, 67], [32, 69], [32, 67], [32, 72], [64, 71], ...] }
```

Drop it in `scores/` and it appears in the **library** menu — notation, set,
bench, playback, MIDI export, all of it. `docs/transcribing.md` is the
procedure: how to read the page furniture, what a pickup bar looks like as a
leading rest, and how to proofread the result.

Because a hand reading miscounts, every set file is checked on load and the
suspicious rows are listed in an amber strip under the score — a pitch off the
keyboard, a duration that is not a writable note value, a bar that does not
fill. They are warnings, not errors: a tie across a barline is real music. The
same report is available at the terminal, which reads the piece back bar by bar
with the notes spelled to the key:

```bash
uv run python -m musicic.core.setfile scores/happy-birthday-melody.music.json
```

It reads the piece back bar by bar — `bar 2  rows 5-6  C5/quarter B4/half` —
which is what you compare against the page. **Save .set** writes the open piece
back out as one, so a correction made in the editor becomes the new
transcription.

`scores/` ships both readings of `2026/music sheets/happy birthday.png`: the
melody as a `pair` set, and both hands as a `triple` set. `triple` is the only
encoding that survives polyphony, so it is the one to use when the
accompaniment matters — but the score is drawn on a single staff, so a piano
texture is engraved approximately even though it plays back exactly.

**Set → score.** Paste a set literal — notebook syntax, `FractionBox`, colour-
highlighted `StyleBox` cells, or plain Python — and it renders as notation and
plays back.

**IC → score, with its structure shown.** Type an indexed concatenation and
watch it expand. Each `€` block gets a colour: its noteheads, its set rows and a
bar in the structure strip all share it, and nesting shows as stacked bars.
Every *copy* of a repeating figure is marked, not just the first. Hover a block
to light up all its copies; click one to select and play just that figure. This
is the derived version of the `Background->RGBColor` marking done by hand in the
notebooks.

```
IC(i=0..7)[{16, 60+2*i}]              rising whole-tone scale
IC(i,0,7)[{16, 60+2*i}]               the same: variable, first, last
IC(i=0,n=7)[{16, 60+2*i}]             the same, limits written as a sum's
IC(i,0,2)[IC(j,i,2)[{8, 60+j}]]       a limit can use an outer index
IC(i=0,n=3)[{16, 60+n-i}]             n is in scope: 63, 62, 61, 60
IC(i=0..1)[{62, 24/3^i}]              the notebook's geometric duration
IC(j=0..3)[{8, 1/6*(j^3-13*j)+81}]    Zelda's cubic -> 81, 79, 78, 79
IC(4)[IC(i=1..3)[{8, 60+i}], {16, 67}] nested
IC(0)[{9,9}]                          vanishes, as the paper requires
IC(i,1,3)[IC(i)[{16, 60+i}]]          a count from an index: 1, 2, then 3 copies
```

`IC(2)[...]` makes two copies; the count can be a formula in an enclosing index,
`IC(n)[...]`, and `IC(0)` -- or any count that comes out 0 or less -- makes
none. The older `IC2[...]` is read the same as `IC(2)[...]`.

`IC` is an ASCII alias for `€`; both are accepted, as is `\[Euro]`. Deep-link an
expression with `?ic=...` or a literal with `?set=...`.

**Hover links the two views.** Point at a set row and its notehead turns red;
point at a notehead and the row outlines, with a readout naming the pitch,
duration and bar. Click to mark a subsequence — the automated form of the
`Background->RGBColor` highlighting in the notebooks — and play just that.

**Compose.** Press **✎ Compose** to write a piece from scratch, or to keep
editing one you just opened. Pick a duration, click a line or space to place
that note at that pitch, click a note to select it. `↑`/`↓` move it a step
(`shift` an octave), `←`/`→` walk the selection, `⌫` deletes, `r` places a rest,
`1`/`2`/`4`/`8` pick a duration, `⌘Z` undoes. Set the bar, tempo and name; the
set and the score update on every edit. **Save .mid** exports a MIDI file that
reads back as exactly the set you wrote.

## Saving and exporting a notebook

**Save** (or ⌘S) in the Notebook writes `notebooks/<name>.md`. It reads on its
own -- the piece, its sets a bar to a line, then every level and its cells with
✓ or ✗ -- and ends with an HTML comment holding the exact notebook as JSON,
which is what **saved…** reopens: the same levels, cells, target and the level
you were on. The ✓ marks are worked out again from the text when the file is
written, not copied from the screen. A Markdown file without that comment still
opens: each ```` ```mathematica ```` block becomes a cell.

**Export PDF…** typesets the notebook with `xelatex` (TeX Live's, from
`/Library/TeX/texbin`): the piece and its score, then level by level each cell
written with € as the paper writes it, its Mathematica form for `iC` in
`reference/SSSiCv102.wl`, the score with that level's blocks in their colours,
and how far each level got. Tick which of those go in. The Mathematica form is
checked against `wolframscript`: it evaluates to the same set.

## Notation

The score is engraved rather than dumped: a key signature is derived (or guessed
from pitch weight when the file has none) and drawn on every system; pitches are
spelled to the key, so an F major piece shows B flat and not A sharp; accidentals
appear only where they differ from the key signature and have not already been
marked in that bar; notes are beamed by beat; and bars are given width in
proportion to how many notes they hold, then justified to the page.

Long pieces are **paged**, eight bars at a time, with `‹ ›` in the Score header.
Drawing several hundred notes in one pass is what made the browser crawl.

## Rests

A rest is a note with **pitch 0**, so a set row stays all-integers and formulas
like `€(i..n)[{16, 60+i}]` keep working -- a symbolic rest would break that. 0 is
not a playable MIDI pitch, so there is no ambiguity. You can write rests in an IC
form directly: `IC(2)[{16,60}, {16,0}, {32,67}]`.

The marker is not uniform across encodings, because 0 is already meaningful in
some of them: `pair`, `pair_flip` and `degree` use `0`; `interval` uses `1000`
(0 is a unison); `pc` uses `-1` (0 is pitch class C).

`triple` and `ioi` carry no rest rows at all, deliberately: they record start
times or onset gaps, so silence is already implied, and rest rows would give two
ways to say the same thing.

## Grid

`GRID` units per quarter note, default **32**, matching the notebooks: 32 =
quarter, 24 = dotted eighth, 16 = eighth, 8 = 16th, 4 = 32nd, 2 = 64th. A grid
of 32 cannot express triplets exactly (an eighth triplet needs 32/3), so pick 48
or 96 for music that has them.

## Encodings

| key | form | exact? |
|---|---|---|
| `triple` | `{start, duration, pitch}` | yes — the only one that survives polyphony |
| `pair` | `{duration, pitch}`, rests as pitch 0 | yes |
| `pair_flip` | `{pitch, duration}` | yes |
| `interval` | `{duration, Δpitch}`, rests as Δ=1000 | yes |
| `ioi` | `{Δstart, pitch}` | rhythm and pitch only, not articulation |
| `ratio` | `{duration ratio ×1000, Δpitch}` | no |
| `contour` | `{duration, −1/0/+1}` | no |
| `degree` | `{duration, diatonic degree}` | no |
| `pc` | `{duration, pitch mod 12}` | no |
| `edsl_pitch` | group by pitch, onset differences within each group | no, nested |
| `edsl_onset` | group by onset, intervals above the bass | no, nested |

The last two are the musical transfer of the paper's edge difference set list:
group the way the EDSL groups edges by originating vertex, then difference
inside each group. They yield a nested list-of-sets — the shape `ReduceSetList`
consumes.

Adding an encoder is a `@register`-decorated function in `core/encoders.py`
returning rows of integers. Set `source_notes` so hover linking keeps working.

## The bench

Every encoder is scored on the loaded track and ranked. `ratio` is a proxy for
how far an IC reduction could collapse the set, built from the same structure
`ReduceSetList` exploits: runs of adjacent duplicates, repeated blocks, and
repeats that survive blanking the varying integers.

Read `ratio` beside `distinct` and the exact/lossy tag — a lossy encoding scores
well partly by discarding information. On the current corpus: Zelda's contour
ratio is 0.047 (256 straight 16ths whose shape repeats), and among lossless
encodings Für Elise does best under `interval`, which cuts its alphabet from 39
distinct values to 26.

## Scope: expansion, not compaction

The tool goes `€ form → set → score`. It does **not** try to derive a `€` form
from a set; you write the form, and the tool shows you what it means and whether
it matches the music.

`core/reduce.py` and `core/findseq.py` contain a working, verified-lossless port
of `ReduceSetList` and a `FindSequenceFunction` equivalent. They are **parked**:
nothing in the app imports them and no endpoint exposes them. They are kept
because they work — on the corpus they reduce 9 of 12 encodings, and on Zelda's
contour they find

    {{8,0}, €49[€3[{8,-1}], {8,1}], €3[{8,-1}]}

200 rows to 4 — but the heuristics that decide *which* structure to prefer are
an open problem, the same 70% the paper reports. Delete both files if you want
them gone; `tests.py` guards only that they never return something that fails to
expand back.

## Known limits

- `{duration, pitch}` cannot express overlapping notes; a legato line is clipped
  before encoding and the count of clipped notes is reported.
- A single-line reading drops chord notes; the count is reported.

## Layout

```
core/notes.py      Note, NoteSet, grid, duration and pitch naming
core/quantize.py   raw ticks -> grid, with an auditable mapping
core/midi_in.py    MIDI -> NoteSets
core/midi_out.py   NoteSet -> MIDI
core/encoders.py   the encoder registry
core/metrics.py    the compressibility bench
core/setfmt.py     set-literal parse and serialize
core/setfile.py    .music.json transcriptions: load, check, save
core/notebookfile.py  notebooks as Markdown, and as LaTeX for the PDF
core/ic.py         IC model, parser, expander
core/score.py      notes -> measures for the renderer
core/reduce.py     PARKED: ReduceSetList port, not wired in
core/findseq.py    PARKED: FindSequenceFunction equivalent
server/app.py      HTTP API
web/               UI; VexFlow 4.2.2 vendored in web/vendor (MIT)
scores/            transcriptions, shown in the library menu
notebooks/         saved notebooks, Markdown with the state inside
docs/transcribing.md   how to read a picture of a score into a set file
```
