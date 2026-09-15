# Transcribing a picture of a score

The tool does not read images. Optical music recognition on a photograph is
unreliable in exactly the way that matters here — a set is only useful if every
integer in it is right — so the reading is done by eye, and the result is
written as a **set file** the app opens like any other piece.

The workflow is: *picture → `.music.json` in `scores/` → open it in the app →
read the warnings → fix → look at the notation beside the picture.*

## 1. Read the page furniture first

Before a single note, settle five things, because every duration in the file
depends on them:

| what | where it goes | how to read it |
|---|---|---|
| time signature | `numerator`, `denominator` | the two stacked numbers after the clef |
| key signature | `key` | count sharps/flats: `"F"`, `"Bb"`, `"Dm"` |
| tempo | `tempo` | the ♩= marking; guess if there is none, it only affects playback |
| smallest note value | `grid` | see below |
| pickup | a leading rest | see below |

**`grid`** is how many units make a quarter note. The default **32** covers
everything down to a 64th note and matches the research notebooks. It *cannot*
express triplets — an eighth triplet needs 32/3 — so if the page has a triplet
bracket anywhere, use **48** (or 96 for triplets plus 32nds) and scale every
duration accordingly. Pick the grid once, for the whole piece.

At grid 32:

| note | units | dotted |
|---|---|---|
| whole | 128 | 192 |
| half | 64 | 96 |
| quarter | 32 | 48 |
| eighth | 16 | 24 |
| 16th | 8 | 12 |
| 32nd | 4 | 6 |

**A pickup (anacrusis) is written as a leading rest** that fills out the
incomplete first bar. A 3/4 bar is 96 units; a two-eighth pickup occupies 32, so
the file opens with `[64, 0]`. Without that rest the barlines are drawn a
pickup's width early for the whole piece — and the check will tell you the last
bar is short.

Look before assuming: `scores/happy-birthday-melody.music.json` is transcribed
from a page that has *no* pickup, because that arrangement puts the two opening
eighths on beat 1. The same tune is barred both ways in print.

## 2. Read the notes

Write `pair` rows: `[duration_in_units, midi_pitch]`, in time order, one bar per
block with a blank line between bars. The blank lines are what make a
transcription proofreadable — you can check each block sums to the bar without
counting past it.

Middle C is **60**. Each semitone is 1, so C4 60, D4 62, E4 64, F4 65, G4 67,
A4 69, B4 71, C5 72. **Apply the key signature**: in F major every B on the page
is B♭ (70, not 71) unless a natural sign says otherwise, and an accidental holds
for the rest of its bar.

**A rest is pitch 0.** That is not a placeholder — it keeps the row all-integers
so an IC formula over the set still works, and 0 is not a playable MIDI pitch,
so there is no ambiguity.

**Ties** are one note of the combined duration: a quarter tied to an eighth is
one `[48, …]` row. **Slurs are not ties** — a slur is phrasing and changes
nothing. **Repeats are written out**; the set is the sounding music.

**Chords.** `pair` is one line, so take the top note and say so in
`transcription.notes`. If the accompaniment matters, write a second file in the
`triple` encoding — `{start, duration, pitch}`, one row per note — which is the
only encoding that survives polyphony. `scores/happy-birthday-piano.music.json`
is that file for the same picture; its rows carry no rests, because start times
already imply the silence.

Transcribing a piece both ways is also the best proofreading there is: the top
line of the `triple` file must come out identical to the `pair` file, and
`tests.py` checks exactly that for the shipped pair.

## 3. Check it before trusting it

```bash
cd 2026/musicic
uv run python -m musicic.core.setfile scores/your-piece.music.json
```

That prints the piece back — bar by bar, with the note names it decoded — and
lists everything suspicious: a pitch off the keyboard, a duration that is not a
writable note value, a note crossing a barline, a last bar that does not fill.

None of those are errors. A tie across a barline is real music and so is a
pickup. They are the places where a hand reading usually goes wrong, so each one
wants a glance back at the picture. The app shows the same list in an amber
strip under the score, and clicking an entry selects that row.

The check that catches the most mistakes is the simplest one: **every bar must
come out the same length.** 3/4 at grid 32 is 96, 4/4 is 128, 6/8 is 96, 2/2 is
128. For a single line that is the sum of the durations in the bar; for a
`triple` file the readback measures how much of the bar is *covered*, since a
chord's notes sound together.

## 4. Look at it

Save into `scores/`, reload the app, pick it from the **library** menu. Compare
the engraved staff to the picture — wrong pitches are obvious as soon as the
contour is drawn, and wrong durations show up as a bar that looks too full.
Press ▶ to hear it; a misread interval is easier to hear than to see.

From there it is an ordinary piece: switch encodings, read the bench, mark
subsequences, write an IC form over it, export MIDI.

## 5. Fixing

Edit the JSON and reload, or open it and press **✎ Compose** to correct notes by
hand, then **Save .set** to write a corrected file back out.

## The format

See the docstring of [`musicic/core/setfile.py`](../musicic/core/setfile.py).
Only `set` is required; everything else defaults. `set` also accepts a set
literal as a string — `"{{32,60},{32,62}}"` — so a block pasted out of a
notebook can go straight in, and a plain `.txt` holding nothing but a literal
opens too.
