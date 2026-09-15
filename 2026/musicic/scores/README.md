# scores/

Transcriptions. Every `.music.json` in here shows up in the **library** picker
in the app, and opens as notation, a set, a bench row and playback.

This is the path a *picture* of a score takes. Nothing in the tool reads an
image: the reading is done by eye, and the result is written here as a set. See
[`../docs/transcribing.md`](../docs/transcribing.md) for how, and for the
checks that catch a miscount before you trust one.

A file is opened by the app the moment it exists — no restart, no upload. Drop
one in, reload the page.

## What is here

Both files are readings of `2026/music sheets/happy birthday.png`:

| file | encoding | what it holds |
|---|---|---|
| `happy-birthday-melody.music.json` | `pair` | the treble line — the one the IC machinery works on |
| `happy-birthday-piano.music.json` | `triple` | both hands, chords included |

The top line of the second is identical to the first; `tests.py` checks it. Two
readings of one picture agreeing is the strongest evidence a hand transcription
offers.
