"""Local HTTP API. Single user, in-memory state -- this is a lab tool."""
from __future__ import annotations

import traceback
import uuid
from dataclasses import asdict, replace
from pathlib import Path

from fastapi import FastAPI, File, Form, HTTPException, UploadFile
from fastapi.responses import FileResponse, JSONResponse, Response
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel

from ..core import encoders as E
from ..core import ic as ICmod
from ..core.metrics import measure
from ..core.keysig import Key
from ..core.midi_in import read_midi
from ..core.midi_out import to_midi
from ..core.notes import GRID, Note, NoteSet, duration_name, pitch_name
from ..core.ic import spans
from ..core.score import layout
from ..core.setfile import SetFile, SetFileError, check as check_setfile, from_noteset
from ..core.setfmt import parse_set, to_mathematica, to_python

WEB = Path(__file__).resolve().parent.parent / "web"
# Transcriptions live beside the package, so a set file written by hand (or by
# an assistant reading a photograph) shows up in the app without an upload.
SCORES = Path(__file__).resolve().parent.parent.parent / "scores"

app = FastAPI(title="musIC")

# session id -> {"sets": [NoteSet], "reports": [...]}
SESSIONS: dict[str, dict] = {}


def _err(exc: Exception) -> JSONResponse:
    return JSONResponse({"error": str(exc), "type": type(exc).__name__}, status_code=400)


PAGE_BARS = 8          # bars drawn at once; long pieces are paged

NO_CACHE = {"Cache-Control": "no-store, must-revalidate", "Pragma": "no-cache"}


@app.get("/")
def index():
    # A lab tool should never serve a stale page after an edit.
    return FileResponse(WEB / "index.html", headers=NO_CACHE)


@app.middleware("http")
async def no_cache_static(request, call_next):
    response = await call_next(request)
    if request.url.path.startswith("/static/") and "vendor" not in request.url.path:
        response.headers.update(NO_CACHE)
    return response


@app.get("/api/encoders")
def encoders():
    return [{
        "key": e.key, "title": e.title, "blurb": e.blurb,
        "columns": list(e.columns), "invertible": e.invertible,
        "nested": e.nested, "caveat": e.caveat,
        "needs_polyphony": e.needs_polyphony,
    } for e in E.REGISTRY.values()]


def _track_payload(ns: NoteSet, report=None) -> dict:
    line, info = ns.monophonic_line()
    out = {
        "name": ns.name, "notes": len(ns), "grid": ns.grid,
        "tempo": ns.tempo_bpm,
        "numerator": ns.numerator, "denominator": ns.denominator,
        "monophonic": ns.is_monophonic(),
        "key": Key(ns.key_root, ns.key_is_minor).display
               if ns.key_root is not None else None,
        "chord_notes_dropped": info["chord_notes_dropped"],
        "notes_clipped": info["notes_clipped"],
    }
    if report is not None:
        out["quantize"] = {
            "ticks_per_beat": report.ticks_per_beat,
            "grid": report.grid,
            "onset_error": round(report.onset_error, 3),
            "rules": report.as_rules(),
            "map": [asdict(s) | {"suspicious": s.suspicious}
                    for s in report.duration_map],
        }
    return out


@app.post("/api/midi")
async def upload_midi(file: UploadFile = File(...), grid: int = Form(GRID),
                      triplets: bool = Form(True)):
    try:
        data = await file.read()
        sets, reports = read_midi(data, grid=grid, allow_triplet=triplets)
        if not sets:
            raise ValueError("no note events found in that file")
        for s in sets:
            s.source = file.filename or "uploaded"
        sid = uuid.uuid4().hex[:12]
        SESSIONS[sid] = {"sets": sets, "reports": reports}
        return {"session": sid, "filename": file.filename,
                "tracks": [_track_payload(s, r) for s, r in zip(sets, reports)]}
    except Exception as exc:                       # noqa: BLE001 - report to UI
        traceback.print_exc()
        return _err(exc)


class ViewReq(BaseModel):
    session: str
    track: int = 0
    encoder: str = "pair"
    reduction: str = "top"        # top | voice0 | full
    max_measures: int | None = PAGE_BARS  # bars per page
    page: int = 0


def _get(sid: str, track: int) -> NoteSet:
    s = SESSIONS.get(sid)
    if not s:
        raise HTTPException(404, "session expired; re-upload the file")
    if not 0 <= track < len(s["sets"]):
        raise HTTPException(404, f"no track {track}")
    return s["sets"][track]


def _encode_payload(ns: NoteSet, key: str) -> dict:
    return _payload_from(E.encode(ns, key), key)


def _payload_from(enc, key: str) -> dict:
    meta = E.REGISTRY[key]
    rows = enc.rows
    m = measure(enc.flat)
    payload = {
        "encoder": key, "title": meta.title, "blurb": meta.blurb,
        "columns": list(enc.columns), "nested": enc.nested,
        "invertible": meta.invertible, "caveat": meta.caveat,
        "labels": enc.labels, "anchor": _jsonable(enc.anchor),
        "metrics": m.as_dict(),
        "mathematica": to_mathematica(rows),
        "python": to_python(rows),
        "rows": [list(r) for r in enc.flat] if not enc.nested
                 else [[list(r) for r in g] for g in rows],
    }
    return payload


def _jsonable(v):
    if isinstance(v, dict):
        return {str(k): _jsonable(x) for k, x in v.items()}
    if isinstance(v, (list, tuple)):
        return [_jsonable(x) for x in v]
    return v


@app.post("/api/view")
def view(req: ViewReq):
    try:
        ns = _get(req.session, req.track)
        if req.reduction == "top":
            base = ns.top_line()
        elif req.reduction.startswith("voice"):
            base = ns.voice(int(req.reduction[5:]))
        else:
            base = ns

        enc = E.encode(base, req.encoder)
        # Draw exactly what the encoding describes, so row indices line up
        # between the staff and the set panel.
        shown = replace(base, notes=[n for n in (enc.source_notes or base.notes)
                                     if n.pitch])
        return {
            "score": layout(base, req.max_measures, enc.source_notes,
                            page=req.page),
            "encoding": _payload_from(enc, req.encoder),
            "playback": _playback(shown),
            "track": _track_payload(ns),
        }
    except HTTPException:
        raise
    except Exception as exc:                       # noqa: BLE001
        traceback.print_exc()
        return _err(exc)


def _playback(ns: NoteSet) -> dict:
    """Flat note list for the browser synth."""
    grid = ns.grid or GRID
    spq = 60.0 / (ns.tempo_bpm or 120)
    return {
        "tempo": ns.tempo_bpm,
        "notes": [{"t": round(n.start / grid * spq, 4),
                   "d": round(n.dur / grid * spq, 4),
                   "p": n.pitch} for n in ns.sorted().notes if n.pitch],
    }


class SetReq(BaseModel):
    text: str
    encoder: str = "pair"
    grid: int = GRID
    tempo: float = 120.0
    numerator: int = 4
    denominator: int = 4
    name: str = "pasted set"


def _notes_from_rows(rows, encoder: str, grid: int) -> list[Note]:
    flat = [r for g in rows for r in g] if rows and isinstance(rows[0], list) \
        and rows[0] and isinstance(rows[0][0], tuple) else rows
    enc = E.Encoding(list(flat), tuple(E.REGISTRY[encoder].columns), encoder,
                     anchor={"first_pitch": 60, "start": 0})
    return E.decode(enc, grid)


@app.post("/api/set")
def from_set(req: SetReq):
    """Parse a pasted set literal and render it."""
    try:
        rows = parse_set(req.text)
        if not rows:
            raise ValueError("nothing to parse")
        meta = E.REGISTRY.get(req.encoder)
        if meta is None or not meta.invertible:
            raise ValueError(f"{req.encoder!r} cannot be read back as notes; "
                             f"choose one of "
                             f"{[k for k, v in E.REGISTRY.items() if v.invertible]}")
        width = len(rows[0]) if rows and isinstance(rows[0], tuple) else 0
        want = len(meta.columns)
        if width != want:
            raise ValueError(f"{req.encoder!r} expects {want} columns "
                             f"{meta.columns}, but the set has {width}")
        notes = _notes_from_rows(rows, req.encoder, req.grid)
        ns = NoteSet(notes=notes, name=req.name, grid=req.grid,
                     tempo_bpm=req.tempo, numerator=req.numerator,
                     denominator=req.denominator, source="pasted")
        sid = uuid.uuid4().hex[:12]
        SESSIONS[sid] = {"sets": [ns], "reports": [None]}
        # Page long pastes too: rendering hundreds of bars at once is what
        # made the browser crawl.
        return {"session": sid, "score": layout(ns, PAGE_BARS),
                "playback": _playback(ns),
                "track": _track_payload(ns), "rows": [list(r) for r in rows],
                "encoding": _encode_payload(ns, req.encoder)}
    except Exception as exc:                       # noqa: BLE001
        traceback.print_exc()
        return _err(exc)


class ICReq(BaseModel):
    text: str
    encoder: str = "pair"
    grid: int = GRID
    tempo: float = 120.0
    numerator: int = 4
    denominator: int = 4


@app.post("/api/ic")
def from_ic(req: ICReq):
    """Expand an indexed-concatenation expression, then render it."""
    try:
        tree = ICmod.parse(req.text)
        expanded = ICmod.expand(tree)
        literal = to_mathematica(expanded)
        inner = SetReq(text=literal, encoder=req.encoder, grid=req.grid,
                       tempo=req.tempo, numerator=req.numerator,
                       denominator=req.denominator, name="from IC")
        out = from_set(inner)
        if isinstance(out, JSONResponse):
            return out
        out["ic"] = {"parsed": ICmod.render(tree), "expanded": literal,
                     "rows": len(expanded),
                     "spans": ICmod.spans(tree),
                     "mathematica": ICmod.render(tree).replace("€", "\\[Euro]")}
        return out
    except Exception as exc:                       # noqa: BLE001
        traceback.print_exc()
        return _err(exc)


class BenchReq(BaseModel):
    session: str
    track: int = 0


@app.post("/api/bench")
def bench(req: BenchReq):
    """Score every encoder on this track, best ratio first."""
    try:
        ns = _get(req.session, req.track)
        rows = []
        for key, meta in E.REGISTRY.items():
            try:
                enc = E.encode(ns, key)
                m = measure(enc.flat)
                rows.append({
                    "encoder": key, "title": meta.title,
                    "invertible": meta.invertible, "nested": meta.nested,
                    "caveat": meta.caveat, "blurb": meta.blurb,
                    "groups": len(enc.rows) if enc.nested else None,
                    **m.as_dict(),
                })
            except Exception as exc:               # noqa: BLE001
                rows.append({"encoder": key, "title": meta.title, "error": str(exc)})
        rows.sort(key=lambda r: (r.get("ratio", 2), -r.get("n", 0)))
        return {"rows": rows}
    except HTTPException:
        raise
    except Exception as exc:                       # noqa: BLE001
        return _err(exc)


class ComposeReq(BaseModel):
    """A piece being written in the editor, as {start, duration, pitch} rows."""
    notes: list[list[int]] = []
    grid: int = GRID
    tempo: float = 120.0
    numerator: int = 4
    denominator: int = 4
    name: str = "new piece"
    encoder: str = "pair"
    session: str | None = None
    page: int = 0


def _compose_set(req: ComposeReq) -> NoteSet:
    notes = [Note(int(s), int(d), int(p)) for s, d, p in req.notes]
    return NoteSet(notes=sorted(notes, key=lambda n: (n.start, n.pitch)),
                   name=req.name, grid=req.grid, tempo_bpm=req.tempo,
                   numerator=req.numerator, denominator=req.denominator,
                   source="composed")


@app.post("/api/compose")
def compose(req: ComposeReq):
    """Re-render a piece being edited. The browser owns the notes; this turns
    them into notation, a set and playback data."""
    try:
        ns = _compose_set(req)
        sid = req.session if req.session in SESSIONS else uuid.uuid4().hex[:12]
        SESSIONS[sid] = {"sets": [ns], "reports": [None]}
        enc = E.encode(ns, req.encoder)
        return {
            "session": sid,
            # One spare bar so there is always somewhere to click.
            "score": layout(ns, PAGE_BARS, enc.source_notes, spare=1,
                            page=max(0, req.page)),
            "encoding": _payload_from(enc, req.encoder),
            "playback": _playback(ns),
            "track": _track_payload(ns),
        }
    except Exception as exc:                       # noqa: BLE001
        traceback.print_exc()
        return _err(exc)


@app.get("/api/export/midi")
def export_midi(session: str, track: int = 0, name: str = "piece"):
    """Download the current piece as a .mid file."""
    ns = _get(session, track)
    safe = "".join(c for c in name if c.isalnum() or c in "-_ ").strip() or "piece"
    return Response(
        content=to_midi(ns), media_type="audio/midi",
        headers={"Content-Disposition": f'attachment; filename="{safe}.mid"'})


@app.get("/api/naming")
def naming(units: int, grid: int = GRID):
    return {"duration": duration_name(units, grid)}


# --------------------------------------------------------------------------
# Set files: a transcription on disk, opened the way a MIDI upload is.
#
# This is the path a picture of a score takes. Nothing here reads an image --
# the transcription is done by eye, outside the app -- but once it is written as
# a .music.json the tool treats it exactly like any other piece: notation, the
# set panel, the bench, playback and MIDI export all work unchanged.
# --------------------------------------------------------------------------

def _adopt_setfile(sf: SetFile, view_encoder: str | None = None) -> dict:
    """Render a set file, and say what looks wrong with it."""
    warnings = check_setfile(sf)
    ns = sf.to_noteset()
    sid = uuid.uuid4().hex[:12]
    SESSIONS[sid] = {"sets": [ns], "reports": [None]}
    key = view_encoder or sf.encoder
    if key not in E.REGISTRY:
        key = sf.encoder
    return {
        "session": sid, "score": layout(ns, PAGE_BARS),
        "playback": _playback(ns), "track": _track_payload(ns),
        "rows": [list(r) for r in sf.rows],
        "encoding": _encode_payload(ns, key),
        "setfile": {"name": sf.name, "encoder": sf.encoder, "grid": sf.grid,
                    "tempo": sf.tempo, "numerator": sf.numerator,
                    "denominator": sf.denominator, "key": sf.key,
                    "source": sf.source, "transcription": sf.transcription,
                    "literal": sf.literal},
        "warnings": [w.as_dict() for w in warnings],
    }


@app.post("/api/setfile")
async def upload_setfile(file: UploadFile = File(...),
                         encoder: str | None = Form(None)):
    """Open an uploaded .music.json, or a text file holding a bare set literal."""
    try:
        raw = (await file.read()).decode("utf-8-sig")
        sf = SetFile.load_text(raw, name_hint=Path(file.filename or "").stem)
        if not sf.source:
            sf.source = file.filename or "uploaded set file"
        return _adopt_setfile(sf, encoder)
    except Exception as exc:                       # noqa: BLE001
        traceback.print_exc()
        return _err(exc)


def _library_files() -> list[Path]:
    if not SCORES.is_dir():
        return []
    out = [p for p in SCORES.rglob("*")
           if p.is_file() and p.suffix.lower() in (".json", ".set", ".txt")
           and not p.name.startswith(".")]
    return sorted(out, key=lambda p: str(p.relative_to(SCORES)).lower())


@app.get("/api/library")
def library():
    """Every transcription sitting in scores/, with whatever each one declares.

    A file that will not parse is listed with its error rather than hidden, so a
    half-finished transcription is visible while it is being fixed.
    """
    rows = []
    for p in _library_files():
        rel = str(p.relative_to(SCORES))
        entry = {"file": rel, "name": p.stem, "error": None}
        try:
            sf = SetFile.load(p)
            entry.update(name=sf.name, encoder=sf.encoder, rows=len(sf.rows),
                         grid=sf.grid, tempo=sf.tempo, key=sf.key,
                         bar=f"{sf.numerator}/{sf.denominator}",
                         source=sf.source,
                         warnings=len(check_setfile(sf)))
        except Exception as exc:                   # noqa: BLE001
            entry["error"] = str(exc)
        rows.append(entry)
    return {"dir": str(SCORES), "files": rows}


class LibraryReq(BaseModel):
    file: str
    encoder: str | None = None


@app.post("/api/library/open")
def library_open(req: LibraryReq):
    try:
        target = (SCORES / req.file).resolve()
        if not str(target).startswith(str(SCORES.resolve()) + "/"):
            raise ValueError("that path is outside the scores directory")
        if not target.is_file():
            raise ValueError(f"no such transcription: {req.file}")
        sf = SetFile.load(target)
        if not sf.source:
            sf.source = req.file
        return _adopt_setfile(sf, req.encoder)
    except Exception as exc:                       # noqa: BLE001
        traceback.print_exc()
        return _err(exc)


@app.get("/api/export/set")
def export_set(session: str, track: int = 0, encoder: str = "pair",
               name: str = "piece"):
    """Download whatever is open as a .music.json, so an edit can be kept."""
    ns = _get(session, track)
    line, _ = ns.monophonic_line()
    sf = from_noteset(line if encoder != "triple" else ns, encoder)
    sf.name = name or sf.name
    safe = "".join(c for c in sf.name if c.isalnum() or c in "-_ ").strip() or "piece"
    return Response(
        content=sf.dumps(), media_type="application/json",
        headers={"Content-Disposition": f'attachment; filename="{safe}.music.json"'})


app.mount("/static", StaticFiles(directory=str(WEB)), name="static")
