"""The .music.json set file: a transcription the website can open.

A set file is the hand-written counterpart to a MIDI upload. It carries a set
literal plus the few facts a set cannot state about itself -- what encoding its
rows are in, how many grid units make a quarter note, the bar, the tempo, the
key -- so a picture of a score can be transcribed once, by eye, and then opened,
re-encoded, benchmarked and played like any other piece.

    {
      "musicic": 1,
      "name": "Happy Birthday",
      "encoder": "pair",
      "grid": 32, "tempo": 108,
      "numerator": 3, "denominator": 4,
      "key": "F",
      "set": [[16, 60], [16, 60], [32, 62], [32, 60], [32, 65], [64, 64]],
      "source": "photo: birthday-page1.jpg"
    }

`set` also accepts a set *literal* as a string, in either notebook or Python
syntax, so a row block copied out of a notebook can be pasted straight in.

A plain-text file holding nothing but a literal is accepted too (`load_text`);
everything it does not say falls back to the defaults above.

Transcription is done by eye and eyes miscount, so `check()` exists: it reports
rows whose duration does not fill the bar, pitches outside the piano, and
anything else that is legal JSON but almost certainly a slip. The server runs it
on every load and hands the warnings to the UI rather than failing.
"""
from __future__ import annotations

import json
from dataclasses import dataclass, field
from fractions import Fraction
from pathlib import Path

from . import encoders as E
from .keysig import LETTER_PC
from .notes import GRID, REST, Note, NoteSet, duration_name
from .setfmt import parse_set, to_mathematica

SUFFIX = ".music.json"
VERSION = 1

# Lowest and highest MIDI pitch on an 88-key piano. REST (0) is also allowed.
PIANO_LOW, PIANO_HIGH = 21, 108


class SetFileError(ValueError):
    """The file cannot be turned into notes at all."""


# --------------------------------------------------------------------------
# key names
# --------------------------------------------------------------------------

def parse_key(text: str | None) -> tuple[int | None, bool]:
    """'F' -> (5, False); 'Dm' / 'D minor' -> (2, True). None stays unknown."""
    if text is None:
        return None, False
    s = str(text).strip()
    if not s:
        return None, False
    minor = False
    low = s.lower()
    for tail in (" minor", "minor", " min", "min", "m"):
        if low.endswith(tail):
            minor, s = True, s[: len(s) - len(tail)].strip()
            break
    else:
        for tail in (" major", "major", " maj", "maj"):
            if low.endswith(tail):
                s = s[: len(s) - len(tail)].strip()
                break
    if not s or s[0].upper() not in LETTER_PC:
        raise SetFileError(f"cannot read key {text!r}; write it like 'F', 'Bb' or 'Dm'")
    pc = LETTER_PC[s[0].upper()]
    for c in s[1:]:
        if c in "#♯":
            pc += 1
        elif c in "b♭":
            pc -= 1
        else:
            raise SetFileError(f"cannot read key {text!r}; write it like 'F', 'Bb' or 'Dm'")
    return pc % 12, minor


def key_text(root: int | None, minor: bool) -> str | None:
    if root is None:
        return None
    from .keysig import Key
    return Key(root, minor).name


# --------------------------------------------------------------------------
# the file
# --------------------------------------------------------------------------

@dataclass
class SetFile:
    """One transcription, as read off disk. `rows` are plain integer tuples."""
    rows: list[tuple] = field(default_factory=list)
    name: str = "untitled"
    encoder: str = "pair"
    grid: int = GRID
    tempo: float = 120.0
    numerator: int = 4
    denominator: int = 4
    key: str | None = None
    source: str = ""
    first_pitch: int = 60          # where an `interval` set starts; ignored otherwise
    transcription: dict = field(default_factory=dict)

    # ---------------------------------------------------------------- read
    @classmethod
    def from_dict(cls, d: dict, *, name_hint: str = "") -> "SetFile":
        if not isinstance(d, dict):
            raise SetFileError("a set file must be a JSON object")
        version = d.get("musicic", VERSION)
        if not isinstance(version, int) or version > VERSION:
            raise SetFileError(
                f"this file says musicic {version!r}; this build reads {VERSION}")

        raw = d.get("set", d.get("rows"))
        if raw is None:
            raise SetFileError('no "set" key -- put the rows there')
        rows = _coerce_rows(raw)
        if not rows:
            raise SetFileError('"set" is empty')

        encoder = str(d.get("encoder", "pair"))
        meta = E.REGISTRY.get(encoder)
        if meta is None:
            raise SetFileError(f"unknown encoder {encoder!r}; have {sorted(E.REGISTRY)}")
        if not meta.invertible:
            playable = [k for k, v in E.REGISTRY.items() if v.invertible]
            raise SetFileError(f"{encoder!r} is lossy and cannot be read back as "
                               f"notes; use one of {playable}")
        want = len(meta.columns)
        width = len(rows[0])
        if width != want:
            raise SetFileError(f"{encoder!r} expects {want} columns {meta.columns}, "
                               f"but the first row has {width}: {list(rows[0])}")
        odd = next((i for i, r in enumerate(rows) if len(r) != want), None)
        if odd is not None:
            raise SetFileError(f"row {odd + 1} has {len(rows[odd])} columns, "
                               f"not {want}: {list(rows[odd])}")

        grid = _positive_int(d.get("grid", GRID), "grid")
        return cls(
            rows=rows,
            name=str(d.get("name") or name_hint or "untitled"),
            encoder=encoder,
            grid=grid,
            tempo=float(d.get("tempo", d.get("tempo_bpm", 120.0))),
            numerator=_positive_int(d.get("numerator", 4), "numerator"),
            denominator=_positive_int(d.get("denominator", 4), "denominator"),
            key=d.get("key"),
            source=str(d.get("source", "")),
            first_pitch=int(d.get("first_pitch", 60)),
            transcription=d.get("transcription") or {},
        )

    @classmethod
    def load_text(cls, text: str, *, name_hint: str = "") -> "SetFile":
        """Read either a .music.json document or a bare set literal."""
        stripped = text.strip()
        if not stripped:
            raise SetFileError("the file is empty")
        if stripped[0] == "{" or stripped[0] == "[":
            try:
                doc = json.loads(stripped)
            except json.JSONDecodeError:
                doc = None            # a notebook literal also starts with '{'
            if isinstance(doc, dict):
                return cls.from_dict(doc, name_hint=name_hint)
            if isinstance(doc, list):
                return cls.from_dict({"set": doc}, name_hint=name_hint)
        return cls.from_dict({"set": stripped}, name_hint=name_hint)

    @classmethod
    def load(cls, path: str | Path) -> "SetFile":
        p = Path(path)
        stem = p.name
        for tail in (SUFFIX, ".json", ".set", ".txt"):
            if stem.endswith(tail):
                stem = stem[: -len(tail)]
                break
        return cls.load_text(p.read_text(encoding="utf-8"), name_hint=stem)

    # --------------------------------------------------------------- write
    def to_dict(self) -> dict:
        out = {
            "musicic": VERSION,
            "name": self.name,
            "encoder": self.encoder,
            "grid": self.grid,
            "tempo": self.tempo,
            "numerator": self.numerator,
            "denominator": self.denominator,
            "set": [list(r) for r in self.rows],
        }
        if self.key:
            out["key"] = self.key
        if self.source:
            out["source"] = self.source
        if self.encoder == "interval":
            out["first_pitch"] = self.first_pitch
        if self.transcription:
            out["transcription"] = self.transcription
        return out

    def dumps(self) -> str:
        """Pretty JSON with one set row per line -- a transcription is edited by
        hand, and a thousand-row array on one line cannot be."""
        d = self.to_dict()
        rows = d.pop("set")
        lines = [f"  {json.dumps(k)}: {json.dumps(v, ensure_ascii=False)},"
                 for k, v in d.items()]
        lines.append('  "set": [')
        lines += [f"    {json.dumps(r)}," for r in rows]
        lines[-1] = lines[-1].rstrip(",")
        lines.append("  ]")
        return "{\n" + "\n".join(lines) + "\n}\n"

    def save(self, path: str | Path) -> Path:
        p = Path(path)
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(self.dumps(), encoding="utf-8")
        return p

    # --------------------------------------------------------------- notes
    def to_noteset(self) -> NoteSet:
        root, minor = parse_key(self.key)
        meta = E.REGISTRY[self.encoder]
        enc = E.Encoding(list(self.rows), tuple(meta.columns), self.encoder,
                         anchor={"first_pitch": self.first_pitch, "start": 0})
        notes = E.decode(enc, self.grid)
        return NoteSet(notes=sorted(notes, key=lambda n: (n.start, n.pitch)),
                       name=self.name, grid=self.grid, tempo_bpm=self.tempo,
                       numerator=self.numerator, denominator=self.denominator,
                       key_root=root, key_is_minor=minor,
                       source=self.source or "set file")

    @property
    def literal(self) -> str:
        return to_mathematica(self.rows)


def _positive_int(v, what: str) -> int:
    try:
        n = int(v)
    except (TypeError, ValueError):
        raise SetFileError(f"{what} must be a whole number, not {v!r}") from None
    if n <= 0:
        raise SetFileError(f"{what} must be positive, not {n}")
    return n


def _coerce_rows(raw) -> list[tuple]:
    """Accept a JSON array of arrays, or a set literal in either dialect."""
    if isinstance(raw, str):
        parsed = parse_set(raw)
    elif isinstance(raw, list):
        parsed = raw
    else:
        raise SetFileError('"set" must be an array of rows or a set literal string')

    rows: list[tuple] = []
    for i, r in enumerate(parsed):
        if isinstance(r, (list, tuple)):
            if r and isinstance(r[0], (list, tuple)):
                raise SetFileError(
                    "this set is nested (a list of groups). Nested encodings are "
                    "views, not sources -- write the piece as flat rows.")
            rows.append(tuple(_as_int(v, i) for v in r))
        else:
            raise SetFileError(f"row {i + 1} is not a list: {r!r}")
    return rows


def _as_int(v, row: int) -> int:
    if isinstance(v, bool):
        raise SetFileError(f"row {row + 1}: true/false is not a number")
    if isinstance(v, int):
        return v
    if isinstance(v, Fraction):
        if v.denominator != 1:
            raise SetFileError(f"row {row + 1}: {v} is not a whole number. Raise "
                               f"`grid` until every duration is an integer.")
        return v.numerator
    if isinstance(v, float):
        if v != int(v):
            raise SetFileError(f"row {row + 1}: {v} is not a whole number. Raise "
                               f"`grid` until every duration is an integer.")
        return int(v)
    raise SetFileError(f"row {row + 1}: {v!r} is not a number")


# --------------------------------------------------------------------------
# checking a transcription
# --------------------------------------------------------------------------

@dataclass
class Warning_:
    """Something suspicious. `row` is 1-based, or None for a whole-piece note."""
    kind: str
    message: str
    row: int | None = None
    bar: int | None = None

    def as_dict(self) -> dict:
        return {"kind": self.kind, "message": self.message,
                "row": self.row, "bar": self.bar}


def check(sf: SetFile) -> list[Warning_]:
    """Report what looks like a miscount, without refusing the file.

    Every check here is a heuristic about *hand* transcription: pitches off the
    keyboard, durations that are not any writable note value, and bars that do
    not add up. None of them are errors -- syncopation ties across barlines and
    pickup bars are real music -- so they are warnings, and the file still opens.
    """
    out: list[Warning_] = []
    ns = sf.to_noteset()
    bar_units = sf.numerator * (4 * sf.grid) // sf.denominator

    for i, n in enumerate(ns.notes, 1):
        if n.dur <= 0:
            out.append(Warning_("duration", f"duration {n.dur} is not positive", i))
            continue
        if n.pitch != REST and not (PIANO_LOW <= n.pitch <= PIANO_HIGH):
            out.append(Warning_(
                "pitch", f"pitch {n.pitch} is off the keyboard "
                         f"({PIANO_LOW}-{PIANO_HIGH}); 0 means a rest", i))
        if duration_name(n.dur, sf.grid).endswith("u"):
            out.append(Warning_(
                "duration", f"{n.dur} units is not a writable note value at "
                            f"grid {sf.grid}; it will be drawn approximately", i))

    # Bar arithmetic. A note crossing a barline is drawn tied, which is correct
    # for syncopation and wrong for a miscounted bar -- so say where it happens
    # and let the eye decide.
    if bar_units > 0:
        for i, n in enumerate(ns.notes, 1):
            first, last = n.start // bar_units, (n.end - 1) // bar_units
            if last > first and n.start % bar_units:
                out.append(Warning_(
                    "barline", f"crosses the barline into bar {last + 1} "
                               f"(tied when drawn)", i, last + 1))
        total = ns.span
        if total % bar_units:
            short = bar_units - total % bar_units
            out.append(Warning_(
                "barline", f"the last bar is {short} of {bar_units} units short "
                           f"({total} units total). A pickup bar explains this; a "
                           f"missed note also does."))
    if sf.key:
        parse_key(sf.key)              # raises on nonsense, early
    return out


# --------------------------------------------------------------------------
# building one from notes, so the app can export what it has open
# --------------------------------------------------------------------------

def from_noteset(ns: NoteSet, encoder: str = "pair", *,
                 source: str = "", transcription: dict | None = None) -> SetFile:
    enc = E.encode(ns, encoder)
    if enc.nested:
        raise SetFileError(f"{encoder!r} produces a nested set; set files are flat")
    return SetFile(
        rows=[tuple(int(v) for v in r) for r in enc.flat],
        name=ns.name, encoder=encoder, grid=ns.grid, tempo=ns.tempo_bpm,
        numerator=ns.numerator, denominator=ns.denominator,
        key=key_text(ns.key_root, ns.key_is_minor),
        first_pitch=int((enc.anchor or {}).get("first_pitch", 60)),
        source=source or ns.source, transcription=transcription or {})


# --------------------------------------------------------------------------
# CLI: read a transcription back in words, so a miscount is visible
#
#   uv run python -m musicic.core.setfile scores/piece.music.json
#
# Reading a transcription aloud, bar by bar, catches what staring at a column of
# integers does not. The bar sums are the useful part: they must all match.
# --------------------------------------------------------------------------

def describe(sf: SetFile) -> str:
    ns = sf.to_noteset()
    bar_units = sf.numerator * (4 * sf.grid) // sf.denominator
    from .keysig import Key
    root, minor = parse_key(sf.key)
    key = Key(root, minor) if root is not None else None
    out = [
        f"{sf.name}",
        f"  {sf.numerator}/{sf.denominator} · {sf.tempo:g} bpm · grid {sf.grid}"
        f" · {Key(root, minor).display if root is not None else 'key unstated'}"
        f" · {sf.encoder}",
        f"  {len(sf.rows)} rows · {ns.span} units · {ns.span / bar_units:g} bars"
        if bar_units else f"  {len(sf.rows)} rows",
        "",
    ]

    bars: dict[int, list[tuple[int, Note]]] = {}
    for i, n in enumerate(ns.notes, 1):
        bars.setdefault(n.start // bar_units if bar_units else 0, []).append((i, n))
    for b in sorted(bars):
        items = bars[b]
        # Measure how much of the bar is covered, not the sum of the durations:
        # a chord's notes sound together, so summing them counts the bar three
        # times over. Union the intervals instead, clipped to this bar.
        lo, hi = b * bar_units, (b + 1) * bar_units
        spans = sorted((max(n.start, lo), min(n.end, hi)) for _, n in items)
        covered, edge = 0, lo
        for a, z in spans:
            if z > edge:
                covered += z - max(a, edge); edge = z
        flag = "" if covered == bar_units else \
               f"   <-- {covered} units covered, not {bar_units}"
        # Group simultaneities, so a chord reads as one entry.
        # Key on (start, duration): notes that begin together but last for
        # different lengths are different events, and merging them would report
        # a duration that only one of them has.
        groups: dict[tuple[int, int], list[Note]] = {}
        for _, n in items:
            groups.setdefault((n.start, n.dur), []).append(n)
        parts = []
        for st, dur in sorted(groups):
            g = sorted(groups[(st, dur)], key=lambda n: n.pitch)
            heads = "+".join(_spelled(n.pitch, key) for n in g)
            parts.append(f"{heads}/{duration_name(dur, sf.grid)}")
        span = f"{items[0][0]}-{items[-1][0]}"
        out.append(f"  bar {b + 1:>3}  rows {span:<9} {' '.join(parts)}{flag}")

    warnings = check(sf)
    out.append("")
    if warnings:
        out.append(f"{len(warnings)} thing(s) to check — guesses about a hand "
                   f"reading, not errors:")
        for w in warnings:
            where = f"row {w.row}" if w.row else "piece"
            out.append(f"  [{w.kind}] {where}: {w.message}")
    else:
        out.append("nothing suspicious.")
    return "\n".join(out)


def _spelled(pitch: int, key) -> str:
    """Pitch name as the key signature would write it. 0 is a rest."""
    if pitch == REST:
        return "rest"
    if key is None:
        from .notes import pitch_name
        return pitch_name(pitch)
    letter, alter, octave = key.spell(pitch)
    mark = {-2: "bb", -1: "b", 0: "", 1: "#", 2: "##"}.get(alter, "")
    return f"{letter}{mark}{octave}"


def _main(argv: list[str] | None = None) -> int:
    import sys
    args = list(sys.argv[1:] if argv is None else argv)
    if not args:
        print(__doc__.strip().splitlines()[0])
        print("usage: python -m musicic.core.setfile <file.music.json> [...]")
        return 2
    bad = 0
    for i, a in enumerate(args):
        if i:
            print()
        try:
            print(describe(SetFile.load(a)))
        except SetFileError as exc:
            print(f"{a}: {exc}")
            bad = 1
    return bad


if __name__ == "__main__":
    raise SystemExit(_main())
