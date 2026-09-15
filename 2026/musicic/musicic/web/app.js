/* musIC — browser side. Renders notation, links it to the set, plays it back. */
const $ = (s) => document.querySelector(s);
const el = (t, cls, txt) => { const n = document.createElement(t);
  if (cls) n.className = cls; if (txt != null) n.textContent = txt; return n; };

const S = {
  session: null, track: 0, encoders: [], view: null,
  fmt: "mathematica", sel: new Set(), noteEls: new Map(), // row -> [svg groups]
  page: 0,
  spans: [], blockOf: new Map(),   // row -> span id, for the structure overlay
  setfile: null, library: null,    // the open transcription, and scores/
  edit: { on: false, notes: [], code: "q", dots: 0, rest: false,
          sel: -1, undo: [] },
  staves: [],                      // geometry, for turning a click into a pitch
};

/* Duration code -> grid units, given units-per-quarter. */
const CODE_Q = { w: 4, h: 2, q: 1, "8": 0.5, "16": 0.25, "32": 0.125 };
const durUnits = (code, dots, grid) => {
  const mult = dots === 2 ? 1.75 : dots === 1 ? 1.5 : 1;
  return Math.round(CODE_Q[code] * grid * mult);
};

/* Staff geometry -> MIDI pitch. The top line of a treble stave is F5, whose
   diatonic number is 5*7+3 = 38; every half line-space below is one step. */
const F5_DIATONIC = 38;
const SEMITONES = [0, 2, 4, 5, 7, 9, 11];
function pitchFromY(y, topLineY, spacing) {
  const step = Math.round((y - topLineY) / (spacing / 2));
  const dn = F5_DIATONIC - step;
  const octave = Math.floor(dn / 7), degree = ((dn % 7) + 7) % 7;
  return Math.min(108, Math.max(21, (octave + 1) * 12 + SEMITONES[degree]));
}

/* Distinct, readable hues for the € blocks. This is the derived version of the
   Background->RGBColor marking done by hand in the notebooks. */
const HUES = [12, 200, 145, 275, 35, 320, 175, 95, 245, 60];
const colorFor = (i) => `hsl(${HUES[i % HUES.length]} 62% 45%)`;
const tintFor = (i) => `hsl(${HUES[i % HUES.length]} 70% 92%)`;

/* ---------------------------------------------------------------- fetch */
async function api(path, body, isForm) {
  const opts = isForm ? { method: "POST", body }
    : { method: "POST", headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body) };
  const r = await fetch(path, opts);
  const j = await r.json().catch(() => ({ error: `${r.status} ${r.statusText}` }));
  if (j && j.error) throw new Error(j.error);
  return j;
}
function status(msg, bad) {
  const n = $("#status"); n.textContent = msg || "";
  n.style.color = bad ? "var(--accent)" : "var(--dim)";
}
function showError(where, msg) {
  const box = el("div", "err", msg);
  where.prepend(box); setTimeout(() => box.remove(), 9000);
}

/* -------------------------------------------------------------- startup */
(async function init() {
  S.encoders = await (await fetch("/api/encoders")).json();
  const sel = $("#encoder");
  for (const e of S.encoders) {
    const o = el("option", null, e.title + (e.invertible ? "" : "  (lossy)"));
    o.value = e.key; sel.append(o);
  }
  sel.value = "pair";
  $("#file").onchange = onFile;          // the label opens the picker natively
  $("#setfile").onchange = (e) => { const f = e.target.files[0]; if (f) onSetFile(f); };
  $("#library").onchange = openFromLibrary;
  loadLibrary();

  // Drag a .mid onto the window as an alternative to the picker.
  const stop = (e) => { e.preventDefault(); e.stopPropagation(); };
  window.addEventListener("dragover", (e) => { stop(e); document.body.classList.add("drop"); });
  window.addEventListener("dragleave", (e) => {
    if (e.relatedTarget === null) document.body.classList.remove("drop");
  });
  window.addEventListener("drop", (e) => {
    stop(e); document.body.classList.remove("drop");
    const f = e.dataTransfer && e.dataTransfer.files[0];
    if (f) (isSetFile(f.name) ? onSetFile(f) : onFile({ target: { files: [f] } }));
  });
  $("#track").onchange = (e) => { S.track = +e.target.value; S.page = 0; refresh(); };
  const turn = (d) => {
    S.page = Math.max(0, S.page + d);
    if (S.edit.on) pushCompose(); else refresh();
  };
  $("#prevPg").onclick = () => turn(-1);
  $("#nextPg").onclick = () => turn(1);
  $("#encoder").onchange = () => { S.page = 0; refresh(); };
  $("#reduction").onchange = () => { S.page = 0; refresh(); };
  $("#grid").onchange = () => { if (S.lastFile) onFile({ target: { files: [S.lastFile] } }); };
  $("#renderSet").onclick = renderSet;
  $("#renderIC").onclick = renderIC;
  $("#icEx").onchange = (e) => { if (e.target.value) { $("#icText").value = e.target.value; renderIC(); } };
  $("#compose").onclick = toggleCompose;
  document.querySelectorAll(".dur").forEach((b) => {
    b.onclick = () => {
      document.querySelectorAll(".dur").forEach((x) =>
        x.setAttribute("aria-selected", x === b));
      S.edit.code = b.dataset.code;
      if (S.edit.sel >= 0) retimeSelected();
    };
  });
  $("#dot").onclick = () => {
    S.edit.dots = S.edit.dots ? 0 : 1;
    $("#dot").setAttribute("aria-selected", !!S.edit.dots);
    if (S.edit.sel >= 0) retimeSelected();
  };
  $("#restBtn").onclick = () => {
    S.edit.rest = !S.edit.rest;
    $("#restBtn").setAttribute("aria-selected", S.edit.rest);
    if (S.edit.rest) addNote(0);        // a rest needs no pitch
  };
  $("#undo").onclick = undoEdit;
  $("#delSel").onclick = deleteSelected;
  $("#clearAll").onclick = () => { pushUndo(); S.edit.notes = []; S.edit.sel = -1; pushCompose(); };
  for (const id of ["numer", "denom", "tempo", "pieceName"])
    $("#" + id).onchange = pushCompose;
  $("#saveSet").onclick = (e) => {
    if (!S.session) { e.preventDefault(); return; }
    // `triple` is the only encoding that survives polyphony, but a hand
    // transcription is a line, so keep whatever is being viewed.
    $("#saveSet").href = `/api/export/set?session=${S.session}&track=${S.track}`
      + `&encoder=${encodeURIComponent($("#encoder").value)}`
      + `&name=${encodeURIComponent($("#pieceName").value || "piece")}`;
  };
  $("#saveMidi").onclick = (e) => {
    if (!S.session) { e.preventDefault(); return; }
    $("#saveMidi").href = `/api/export/midi?session=${S.session}&track=0`
      + `&name=${encodeURIComponent($("#pieceName").value || "piece")}`;
  };
  document.addEventListener("keydown", onKey);
  $("#play").onclick = play;
  $("#stop").onclick = stopAll;
  $("#copy").onclick = copyLiteral;
  document.querySelectorAll(".tabs button").forEach((b) => {
    b.onclick = () => {
      document.querySelectorAll(".tabs button").forEach((x) =>
        x.setAttribute("aria-selected", x === b));
      S.fmt = b.dataset.fmt; drawLiteral();
    };
  });
  // Click empty staff space to append a note at that pitch.
  $("#score").addEventListener("click", (ev) => {
    if (!S.edit.on || !S.staves.length) return;
    const svg = $("#score").querySelector("svg");
    if (!svg) return;
    const box = svg.getBoundingClientRect();
    const x = ev.clientX - box.left, y = ev.clientY - box.top;
    // Nearest stave by vertical distance, so clicks between systems still land.
    let best = S.staves[0], bd = Infinity;
    for (const s of S.staves) {
      const d = Math.abs(y - (s.top + s.spacing * 2));
      if (d < bd) { bd = d; best = s; }
    }
    S.edit.rest = false; $("#restBtn").setAttribute("aria-selected", false);
    addNote(pitchFromY(y, best.top, best.spacing));
  });

  window.addEventListener("resize", () => S.view && drawScore(S.view.score));

  // Deep links: ?ic=<expression> or ?set=<literal>, so an expression you are
  // working on can be bookmarked or pasted to a colleague.
  const q = new URLSearchParams(location.search);
  if (q.get("encoder")) $("#encoder").value = q.get("encoder");
  if (q.get("grid")) $("#grid").value = q.get("grid");
  if (q.get("ic")) { $("#icText").value = q.get("ic"); await renderIC(); }
  else if (q.get("set")) { $("#setText").value = q.get("set"); await renderSet(); }
  else status("Open a MIDI file, pick a transcription from the library, "
              + "or build a set on the right.");
})();

/* ------------------------------------------------------------ MIDI load */
async function onFile(e) {
  const f = e.target.files[0]; if (!f) return;
  S.lastFile = f;
  status("reading " + f.name + "…");
  const fd = new FormData();
  fd.append("file", f);
  fd.append("grid", $("#grid").value);
  try {
    const j = await api("/api/midi", fd, true);
    S.session = j.session; S.track = 0; S.setfile = null;
    drawWarnings([]);
    const t = $("#track"); t.innerHTML = "";
    j.tracks.forEach((tr, i) => {
      const o = el("option", null, `${tr.name} — ${tr.notes} notes${tr.monophonic ? "" : " (poly)"}`);
      o.value = i; t.append(o);
    });
    status(`${j.filename}: ${j.tracks.length} track(s)`);
    await refresh();
  } catch (err) { status(err.message, true); showError($("#score"), err.message); }
}

/* -------------------------------------------------- set files & library */
/* A set file is a transcription: a set literal plus the bar, grid, tempo and
   key that a bare set cannot state about itself. It is how a photographed
   score reaches this tool -- the reading is done by eye, elsewhere, and the
   result is opened here like any other piece. */
const isSetFile = (n) => /\.(json|set|txt)$/i.test(n || "");

async function onSetFile(f) {
  status("reading " + f.name + "…");
  const fd = new FormData();
  fd.append("file", f);
  try {
    const j = await api("/api/setfile", fd, true);
    adopt(j);
    status(`${j.setfile.name}: ${j.rows.length} rows`
      + (j.warnings.length ? ` · ${j.warnings.length} to check` : ""));
  } catch (err) { status(err.message, true); showError($("#score"), err.message); }
}

async function loadLibrary(keep) {
  const sel = $("#library");
  try {
    const j = await fetch("/api/library").then((r) => r.json());
    S.library = j;
    sel.innerHTML = "";
    const head = el("option", null,
      j.files.length ? `${j.files.length} in scores/…` : "scores/ is empty");
    head.value = ""; sel.append(head);
    for (const f of j.files) {
      const o = el("option", null, f.error
        ? `${f.file} — will not parse`
        : `${f.name} · ${f.rows} rows · ${f.bar}`
          + (f.warnings ? ` · ${f.warnings}?` : ""));
      o.value = f.file; o.title = f.error || f.source || f.file;
      sel.append(o);
    }
    if (keep) sel.value = keep;
  } catch (err) { sel.innerHTML = "<option value=''>library unavailable</option>"; }
}

async function openFromLibrary() {
  const file = $("#library").value; if (!file) return;
  status("opening " + file + "…");
  try {
    const j = await api("/api/library/open", { file });
    adopt(j);
    status(`${j.setfile.name}: ${j.rows.length} rows`
      + (j.warnings.length ? ` · ${j.warnings.length} to check` : ""));
  } catch (err) { status(err.message, true); showError($("#score"), err.message); }
}

/* The checks are advisory: a tie across a barline and a pickup bar are real
   music, so a warning points at a row and lets the eye settle it. */
function drawWarnings(list, sf) {
  const box = $("#warnBox");
  if (!list || !list.length) { box.hidden = true; box.innerHTML = ""; return; }
  box.hidden = false; box.innerHTML = "";
  const head = el("div");
  head.append(el("b", null, `${list.length} thing${list.length > 1 ? "s" : ""} to check`));
  head.append(document.createTextNode(
    sf && sf.source ? ` in ${sf.source} — these are guesses about a hand reading, not errors.`
                    : " — these are guesses about a hand reading, not errors."));
  box.append(head);
  const ul = el("ul");
  for (const w of list) {
    const li = el("li");
    li.append(el("span", "kind", w.kind));
    li.append(document.createTextNode(
      (w.row ? `row ${w.row}: ` : "") + w.message));
    if (w.row) li.onclick = () => {
      S.sel.clear(); S.sel.add(w.row - 1); syncSel(); scrollToRow(w.row - 1); };
    ul.append(li);
  }
  box.append(ul);
}

function scrollToRow(i) {
  const n = document.querySelector(`#rows .row[data-row="${i}"]`);
  if (n) n.scrollIntoView({ block: "nearest", inline: "nearest" });
}

/* -------------------------------------------------------------- refresh */
async function refresh() {
  if (!S.session) return;
  try {
    const j = await api("/api/view", {
      session: S.session, track: S.track, page: S.page,
      encoder: $("#encoder").value, reduction: $("#reduction").value,
    });
    S.view = j; S.sel.clear(); setSpans([]);
    drawScore(j.score); drawRows(j.encoding); drawLiteral();
    drawQuant(j.track); drawMeta(j.track, j.encoding);
    paintBlocks(); bench();
  } catch (err) { status(err.message, true); showError($("#score"), err.message); }
}

function drawMeta(track, enc) {
  const k = S.view && S.view.score && S.view.score.key_display;
  $("#scoreMeta").textContent =
    `${track.name} · ${track.numerator}/${track.denominator} · ${track.tempo} bpm`
    + (k ? ` · ${k}` : "");
  const m = enc.metrics;
  $("#setMeta").textContent =
    `${m.n} rows · ${m.distinct} distinct · ratio ${m.ratio}`;
  const bits = [enc.blurb];
  if (enc.caveat) bits.push("Caveat: " + enc.caveat + ".");
  if (track.chord_notes_dropped)
    bits.push(`${track.chord_notes_dropped} chord notes dropped by this single-line reading.`);
  if (track.notes_clipped)
    bits.push(`${track.notes_clipped} overlapping notes clipped — this form cannot express overlap.`);
  $("#blurb").textContent = bits.join(" ");
}

/* ---------------------------------------------------------------- score */
function drawScore(score) {
  const host = $("#score"); host.innerHTML = ""; S.noteEls = new Map();
  S.staves = [];
  if (!score || !score.measures.length) {
    host.append(el("div", "body sub", "nothing to draw")); return;
  }
  const VF = Vex.Flow;
  const width = Math.max(host.clientWidth - 4, 480);
  const usable = width - 24;
  const FIRST_EXTRA = 74;          // clef + key signature + time signature
  const lineH = 118;

  // Give each bar room in proportion to what is in it, then justify each system
  // to the full width. Packing a fixed number of bars per line is what let a
  // dense bar collapse into a smear of overlapping noteheads.
  const want = score.measures.map((m) => {
    const n = m.notes.length || 1;
    return Math.max(96, 34 + 26 * n);
  });

  const lines = [];
  let cur = [], curW = 0;
  score.measures.forEach((m, i) => {
    const extra = cur.length === 0 ? FIRST_EXTRA : 0;
    if (cur.length && curW + want[i] + extra > usable) { lines.push(cur); cur = []; curW = 0; }
    cur.push(i); curW += want[i] + (cur.length === 1 ? FIRST_EXTRA : 0);
  });
  if (cur.length) lines.push(cur);

  const r = new VF.Renderer(host, VF.Renderer.Backends.SVG);
  r.resize(width, lines.length * lineH + 26);
  const ctx = r.getContext(); ctx.setFont("sans-serif", 9);

  lines.forEach((idxs, row) => {
    // Justify: scale this line's bars so they exactly span the page.
    const extra = FIRST_EXTRA;
    const raw = idxs.reduce((s, i) => s + want[i], 0);
    const scale = Math.max(0.55, (usable - extra) / Math.max(raw, 1));
    let x = 12;
    idxs.forEach((i, col) => {
      const m = score.measures[i];
      const w = want[i] * scale + (col === 0 ? extra : 0);
      const stave = new VF.Stave(x, 12 + row * lineH, w);
      if (col === 0) {
        stave.addClef("treble");
        if (score.key && score.key !== "C") stave.addKeySignature(score.key);
        if (row === 0) stave.addTimeSignature(`${score.numerator}/${score.denominator}`);
      }
      stave.setContext(ctx).draw();
      S.staves.push({ x, w, top: stave.getYForLine(0),
                      spacing: stave.getYForLine(1) - stave.getYForLine(0) });

      const notes = m.notes.map((n) => {
        const sn = new VF.StaveNote({
          keys: n.keys, duration: n.code + (n.rest ? "r" : ""),
          clef: "treble", autoStem: true,
        });
        for (let d = 0; d < n.dots; d++) VF.Dot.buildAndAttach([sn], { all: true });
        // Only the accidentals the layout said to draw: the key signature and
        // the rest of the bar cover the others.
        (n.accidentals || []).forEach((a, k) => {
          if (a) sn.addModifier(new VF.Accidental(a), k);
        });
        sn._rows = n.rows || []; sn._label = n.label;
        sn._code = n.code; sn._dots = n.dots;
        return sn;
      });

      let beams = [];
      try {
        const voice = new VF.Voice({ num_beats: score.numerator,
          beat_value: score.denominator }).setStrict(false);
        voice.addTickables(notes);
        // Beam by beat, the way an engraver would: a run of sixteenths in 4/4
        // breaks into groups of four rather than one bar-long beam.
        beams = VF.Beam.generateBeams(notes.filter((n) => !n.isRest()), {
          groups: [new VF.Fraction(1, score.denominator)],
        });
        new VF.Formatter().joinVoices([voice]).format([voice],
          Math.max(w - (col === 0 ? extra + 16 : 18), 60));
        voice.draw(ctx, stave);
        beams.forEach((b) => b.setContext(ctx).draw());
      } catch (e) { /* one bad bar must not blank the page */ }

      notes.forEach((sn) => {
        const g = sn.getSVGElement && sn.getSVGElement();
        if (!g || !sn._rows.length) return;
        g.classList.add("vf-note");
        g.dataset.rows = sn._rows.join(",");
        for (const rw of sn._rows) {
          if (!S.noteEls.has(rw)) S.noteEls.set(rw, []);
          S.noteEls.get(rw).push(g);
        }
        g.addEventListener("mouseenter", () => hover(sn._rows[0]));
        g.addEventListener("mouseleave", () => hover(null));
        g.addEventListener("click", (ev) => {
          if (S.edit.on) { ev.stopPropagation(); S.edit.sel = sn._rows[0]; markSel(); }
          else sn._rows.forEach(toggleSel);
        });
      });
      x += w;
    });
  });

  drawPager(score);
  syncSel();            // re-apply selection colours to the new noteheads
}

function drawPager(score) {
  const p = score.page || 0, n = score.pages || 1;
  $("#pager").textContent = n > 1
    ? `bars ${p * score.per_page + 1}\u2013${Math.min((p + 1) * score.per_page,
        score.total_measures)} of ${score.total_measures}` : "";
  $("#prevPg").disabled = p <= 0;
  $("#nextPg").disabled = p >= n - 1;
  $("#prevPg").style.display = $("#nextPg").style.display = n > 1 ? "" : "none";
}

/* ------------------------------------------------------------------ set */
function drawRows(enc) {
  const host = $("#rows"); host.innerHTML = "";
  const mk = (r, i) => {
    const n = el("span", "row", "{" + r.join(",") + "}");
    n.dataset.row = i;
    n.addEventListener("mouseenter", () => hover(i, null));
    n.addEventListener("mouseleave", () => hover(null));
    n.addEventListener("click", () => toggleSel(i));
    return n;
  };
  if (enc.nested) {
    enc.rows.forEach((g, gi) => {
      const d = el("div", "grp");
      d.append(el("span", "lbl", (enc.labels && enc.labels[gi]) || `#${gi}`));
      g.forEach((r) => d.append(mk(r, -1)));
      if (!g.length) d.append(el("span", "row sub", "{}"));
      host.append(d);
    });
  } else {
    enc.rows.forEach((r, i) => host.append(mk(r, i)));
  }
}

function drawLiteral() {
  if (!S.view) return;
  $("#literal").textContent = S.view.encoding[S.fmt];
}
async function copyLiteral() {
  try { await navigator.clipboard.writeText($("#literal").textContent);
    status("copied"); } catch { status("copy blocked by the browser", true); }
}

/* -------------------------------------------------------------- linking */
function hover(row, label) {
  document.querySelectorAll(".hot").forEach((n) => n.classList.remove("hot"));
  if (row == null) { $("#readout").textContent = "Hover a note or a set row to link them."; return; }
  (S.noteEls.get(row) || []).forEach((g) => g.classList.add("hot"));
  const rowEl = document.querySelector(`#rows .row[data-row="${row}"]`);
  if (rowEl) { rowEl.classList.add("hot");
    rowEl.scrollIntoView({ block: "nearest", inline: "nearest" }); }
  const enc = S.view && S.view.encoding;
  const vals = enc && !enc.nested && enc.rows[row];
  const cols = enc ? enc.columns : [];
  const desc = vals ? cols.map((c, i) => `${c} ${vals[i]}`).join(" · ") : "";
  const note = noteInfo(row);
  $("#readout").innerHTML = `<b>row ${row}</b> &nbsp; ${esc(desc)}`
    + (note ? ` &nbsp;→&nbsp; <b>${esc(note)}</b>` : "");
}
function noteInfo(row) {
  const sc = S.view && S.view.score; if (!sc) return "";
  for (const m of sc.measures) for (const n of m.notes)
    if ((n.rows || []).includes(row))
      return `${n.label} · ${n.code}${".".repeat(n.dots)} · bar ${m.index + 1}`;
  return "";
}
function esc(s) { return String(s).replace(/[&<>]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;" }[c])); }

function toggleSel(row) {
  if (S.sel.has(row)) S.sel.delete(row); else S.sel.add(row);
  syncSel();
}

function syncSel() {
  document.querySelectorAll("#rows .row").forEach((n) =>
    n.classList.toggle("sel", S.sel.has(+n.dataset.row)));
  // Mirror the selection onto the staff, so picking rows marks the music.
  document.querySelectorAll(".picked").forEach((n) => n.classList.remove("picked"));
  for (const row of S.sel)
    (S.noteEls.get(row) || []).forEach((g) => g.classList.add("picked"));
  const enc = S.view && S.view.encoding;
  if (!enc) return;
  const idx = [...S.sel].sort((a, b) => a - b);
  $("#selInfo").textContent = idx.length
    ? `${idx.length} selected — ${enc.mathematica ? "{" + idx.map((i) =>
        "{" + enc.rows[i].join(",") + "}").join(",") + "}" : ""}`.slice(0, 160)
    : "Click rows or noteheads to mark a subsequence.";
}

/* --------------------------------------------------------------- compose */
function toggleCompose() {
  S.edit.on = !S.edit.on;
  $("#palette").hidden = !S.edit.on;
  $("#score").classList.toggle("editing", S.edit.on);
  $("#compose").setAttribute("aria-selected", S.edit.on);
  $("#compose").style.background = S.edit.on ? "var(--accent)" : "";
  $("#compose").style.color = S.edit.on ? "#fff" : "";
  if (S.edit.on) {
    // Seed the editor from whatever is on screen, so you can open a MIDI and
    // keep writing rather than starting from nothing.
    if (!S.edit.notes.length && S.view && S.view.score) {
      S.edit.notes = notesFromView();
    }
    pushCompose();
  }
}

function notesFromView() {
  const enc = S.view && S.view.encoding;
  if (!enc || enc.nested) return [];
  const grid = (S.view.track && S.view.track.grid) || 32;
  let t = 0; const out = [];
  for (const r of enc.rows) {
    if (enc.encoder === "triple") { out.push([r[0], r[1], r[2]]); continue; }
    const [d, p] = enc.encoder === "pair_flip" ? [r[1], r[0]] : [r[0], r[1]];
    out.push([t, d, p]); t += d;
  }
  return out;
}

function pushUndo() {
  S.edit.undo.push(JSON.stringify(S.edit.notes));
  if (S.edit.undo.length > 60) S.edit.undo.shift();
}
function undoEdit() {
  const prev = S.edit.undo.pop();
  if (prev == null) return;
  S.edit.notes = JSON.parse(prev); S.edit.sel = -1; pushCompose(true);
}

function pieceEnd() {
  return S.edit.notes.reduce((m, n) => Math.max(m, n[0] + n[1]), 0);
}

function addNote(pitch) {
  const grid = +$("#grid").value || 32;
  pushUndo();
  const d = durUnits(S.edit.code, S.edit.dots, grid);
  S.edit.notes.push([pieceEnd(), d, pitch]);
  S.edit.sel = S.edit.notes.length - 1;
  pushCompose();
}

function retimeSelected() {
  const i = S.edit.sel; if (i < 0) return;
  const grid = +$("#grid").value || 32;
  pushUndo();
  S.edit.notes[i][1] = durUnits(S.edit.code, S.edit.dots, grid);
  relayout(); pushCompose();
}

function relayout() {
  // Keep the line sequential: each note starts where the previous ended.
  S.edit.notes.sort((a, b) => a[0] - b[0]);
  let t = 0;
  for (const n of S.edit.notes) { n[0] = t; t += n[1]; }
}

function deleteSelected() {
  const i = S.edit.sel;
  pushUndo();
  if (i >= 0) S.edit.notes.splice(i, 1); else S.edit.notes.pop();
  relayout(); S.edit.sel = -1; pushCompose();
}

function onKey(e) {
  if (!S.edit.on) return;
  const tag = (e.target.tagName || "").toLowerCase();
  if (tag === "input" || tag === "textarea" || tag === "select") return;
  const i = S.edit.sel;
  if (e.key === "Backspace" || e.key === "Delete") { e.preventDefault(); deleteSelected(); }
  else if (e.key === "ArrowUp" && i >= 0) { e.preventDefault(); nudge(i, e.shiftKey ? 12 : 1); }
  else if (e.key === "ArrowDown" && i >= 0) { e.preventDefault(); nudge(i, e.shiftKey ? -12 : -1); }
  else if (e.key === "ArrowLeft") { e.preventDefault(); S.edit.sel = Math.max(0, i - 1); markSel(); }
  else if (e.key === "ArrowRight") { e.preventDefault(); S.edit.sel = Math.min(S.edit.notes.length - 1, i + 1); markSel(); }
  else if (e.key === "r") { $("#restBtn").click(); }
  else if (e.key === "z" && (e.metaKey || e.ctrlKey)) { e.preventDefault(); undoEdit(); }
  else if ("1245".includes(e.key)) {
    const map = { 1: "w", 2: "h", 4: "q", 8: "8" };
    const b = document.querySelector(`.dur[data-code="${map[e.key] || "q"}"]`);
    if (b) b.click();
  }
}

function nudge(i, by) {
  pushUndo();
  const p = S.edit.notes[i][2];
  if (p) S.edit.notes[i][2] = Math.min(108, Math.max(21, p + by));
  pushCompose();
}

function markSel() {
  document.querySelectorAll(".selnote").forEach((n) => n.classList.remove("selnote"));
  (S.noteEls.get(S.edit.sel) || []).forEach((g) => g.classList.add("selnote"));
}

async function pushCompose(skipUndo) {
  try {
    const j = await api("/api/compose", {
      notes: S.edit.notes, grid: +$("#grid").value || 32,
      tempo: +$("#tempo").value || 120,
      numerator: +$("#numer").value || 4,
      denominator: +$("#denom").value || 4,
      name: $("#pieceName").value || "new piece",
      encoder: $("#encoder").value, session: S.session, page: S.page,
    });
    S.session = j.session; S.view = j; setSpans([]);
    drawScore(j.score); drawRows(j.encoding); drawLiteral();
    drawMeta(j.track, j.encoding); drawQuant(j.track); markSel(); bench();
    status(`${S.edit.notes.length} notes · ${$("#pieceName").value}`);
  } catch (err) { status(err.message, true); }
}

/* ------------------------------------------------------ structure overlay */
function setSpans(spans) {
  S.spans = spans || [];
  S.blockOf = new Map();
  // One hue per distinct node, and each row takes the colour of the *deepest*
  // block containing it. Colouring by the outermost block instead would paint
  // the whole piece one colour whenever a single € wraps everything, which
  // tells you nothing.
  S.ids = [...new Set(S.spans.map((s) => s.id))];
  for (const s of [...S.spans].sort((a, b) => a.depth - b.depth)) {
    const c = S.ids.indexOf(s.id);
    for (let r = s.start; r <= s.end; r++) S.blockOf.set(r, c);
  }
  drawStructure();
}

const idColor = (id) => colorFor(S.ids.indexOf(id));

function drawStructure() {
  const host = $("#icSpans");
  if (!S.spans.length) { host.hidden = true; host.innerHTML = ""; return; }
  host.hidden = false; host.innerHTML = "";
  const total = Math.max(...S.spans.map((s) => s.end)) + 1;
  const byDepth = new Map();
  for (const s of S.spans) {
    if (!byDepth.has(s.depth)) byDepth.set(s.depth, []);
    byDepth.get(s.depth).push(s);
  }
  // One bar row per nesting depth, each block drawn at its true extent.
  const strip = el("div"); strip.id = "strip";
  for (const [depth, list] of [...byDepth].sort((a, b) => a[0] - b[0])) {
    const row = el("div", "striprow");
    row.style.position = "relative";
    for (const s of list) {
      const seg = el("div");
      seg.style.cssText = `position:absolute;left:${(s.start / total) * 100}%;`
        + `width:${((s.end - s.start + 1) / total) * 100}%;height:7px;`
        + `border-radius:2px;background:${idColor(s.id)}`;
      seg.title = `${s.label}  rows ${s.start}-${s.end}`;
      seg.onmouseenter = () => highlightRange(s.start, s.end);
      seg.onmouseleave = () => highlightRange(null);
      seg.onclick = () => { S.sel.clear();
        for (let r = s.start; r <= s.end; r++) S.sel.add(r);
        syncSel(); play(); };
      row.append(seg);
    }
    strip.append(row);
  }
  host.append(strip);

  const uniq = [];
  for (const s of S.spans) if (!uniq.some((u) => u.id === s.id)) uniq.push(s);
  for (const s of uniq) {
    const b = el("div", "blk");
    const sw = el("span", "sw");
    sw.style.background = idColor(s.id);
    b.append(sw);
    b.append(el("span", null, "  ".repeat(s.depth) + s.text.slice(0, 46)));
    const copies = S.spans.filter((x) => x.id === s.id).length;
    b.append(el("span", "rng", copies > 1 ? `${copies} copies` : `${s.rows} rows`));
    b.onmouseenter = () => {
      for (const x of S.spans) if (x.id === s.id) highlightRange(x.start, x.end, true);
    };
    b.onmouseleave = () => highlightRange(null);
    b.onclick = () => { S.sel.clear();
      for (const x of S.spans) if (x.id === s.id)
        for (let r = x.start; r <= x.end; r++) S.sel.add(r);
      syncSel(); play(); };
    host.append(b);
  }
}

function highlightRange(a, b, add) {
  if (!add) document.querySelectorAll(".hot").forEach((n) => n.classList.remove("hot"));
  if (a == null) return;
  for (let r = a; r <= b; r++) {
    (S.noteEls.get(r) || []).forEach((g) => g.classList.add("hot"));
    const e = document.querySelector(`#rows .row[data-row="${r}"]`);
    if (e) e.classList.add("hot");
  }
  $("#readout").innerHTML = `<b>rows ${a}–${b}</b> &nbsp; ${b - a + 1} notes`;
}

function paintBlocks() {
  if (!S.blockOf.size) return;
  for (const [row, c] of S.blockOf) {
    const e = document.querySelector(`#rows .row[data-row="${row}"]`);
    if (e) { e.style.background = tintFor(c); e.style.borderColor = colorFor(c); }
    (S.noteEls.get(row) || []).forEach((g) => {
      g.querySelectorAll("path").forEach((pp) => { pp.style.fill = colorFor(c); });
    });
  }
}

/* ---------------------------------------------------------------- build */
async function renderSet() {
  const text = $("#setText").value.trim(); if (!text) return;
  try {
    const j = await api("/api/set", { text, encoder: $("#encoder").value,
      grid: +$("#grid").value });
    adopt(j); status(`rendered ${j.rows.length} rows`);
  } catch (err) { showError($("#setText").parentElement, err.message); status(err.message, true); }
}
async function renderIC() {
  const text = $("#icText").value.trim(); if (!text) return;
  try {
    const j = await api("/api/ic", { text, encoder: $("#encoder").value,
      grid: +$("#grid").value });
    adopt(j);
    const o = $("#icOut"); o.hidden = false;
    o.textContent = `${j.ic.parsed}\n→ ${j.ic.expanded}`;
    status(`expanded to ${j.ic.rows} rows`);
  } catch (err) { showError($("#icText").parentElement, err.message); status(err.message, true); }
}
function adopt(j) {
  S.session = j.session; S.track = 0; S.view = j; S.sel.clear();
  setSpans(j.ic ? j.ic.spans : []);
  // A set file states its own encoding, bar and tempo; adopt them, so the panel
  // shows the rows as they were written rather than re-encoded into whatever
  // the picker last held.
  if (j.setfile) {
    const sf = j.setfile;
    if (sf.encoder) $("#encoder").value = sf.encoder;
    if (sf.grid && $("#grid").querySelector(`option[value="${sf.grid}"]`))
      $("#grid").value = sf.grid;
    $("#pieceName").value = sf.name || "piece";
    $("#tempo").value = Math.round(sf.tempo || 120);
    $("#numer").value = sf.numerator || 4;
    $("#denom").value = sf.denominator || 4;
    S.setfile = sf;
  } else {
    S.setfile = null;
  }
  drawWarnings(j.warnings, j.setfile);
  const t = $("#track"); t.innerHTML = "";
  const o = el("option", null, j.track.name); o.value = 0; t.append(o);
  drawScore(j.score); drawRows(j.encoding); drawLiteral();
  drawMeta(j.track, j.encoding); drawQuant(j.track); paintBlocks(); bench();
}

/* ---------------------------------------------------------------- bench */
async function bench() {
  if (!S.session) return;
  const tb = $("#bench").querySelector("tbody"); tb.innerHTML = "";
  const head = el("tr");
  ["encoding", "n", "distinct", "runs", "ratio"].forEach((h, i) => {
    const th = el("th", i > 0 ? "num" : null, h); head.append(th);
  });
  tb.append(head);
  try {
    const j = await api("/api/bench", { session: S.session, track: S.track });
    for (const r of j.rows) {
      const tr = el("tr", "pick");
      if (r.encoder === $("#encoder").value) tr.classList.add("on");
      const name = el("td");
      name.append(document.createTextNode(r.title));
      const tag = el("span", "tag " + (r.error ? "" : r.invertible ? "exact" : "lossy"),
        r.error ? "error" : r.invertible ? "exact" : "lossy");
      name.append(tag);
      if (r.nested) name.append(el("span", "tag", "nested"));
      tr.append(name);
      if (r.error) {
        const td = el("td", "num sub", r.error); td.colSpan = 4; tr.append(td);
      } else {
        [r.n, r.distinct, r.runs, r.ratio].forEach((v) => tr.append(el("td", "num", v)));
      }
      tr.title = r.blurb || "";
      tr.onclick = () => { $("#encoder").value = r.encoder; refresh(); };
      tb.append(tr);
    }
  } catch (err) { /* bench is advisory */ }
}

/* -------------------------------------------------------------- quantize */
function drawQuant(track) {
  const tb = $("#qmap").querySelector("tbody"); tb.innerHTML = "";
  const q = track.quantize;
  if (!q) { $("#qMeta").textContent = "n/a — set was entered directly";
    $("#qrules").textContent = ""; return; }
  $("#qMeta").textContent = `ppq ${q.ticks_per_beat} → grid ${q.grid}`;
  const head = el("tr");
  ["raw", "→", "value", "count", "moved"].forEach((h, i) =>
    head.append(el("th", i === 0 || i > 2 ? "num" : null, h)));
  tb.append(head);
  for (const s of q.map) {
    const tr = el("tr");
    if (s.suspicious) tr.style.color = "var(--warn)";
    tr.append(el("td", "num", s.raw));
    tr.append(el("td", "num", "→"));
    tr.append(el("td", null, `${s.units}  ${s.name}`));
    tr.append(el("td", "num", "×" + s.count));
    tr.append(el("td", "num", (s.error * 100).toFixed(1) + "%"));
    tb.append(tr);
  }
  $("#qrules").textContent = q.rules;
}

/* -------------------------------------------------------------- playback */
let AC = null, playing = [];
function ctx() { AC = AC || new (window.AudioContext || window.webkitAudioContext)(); return AC; }
function stopAll() { playing.forEach((o) => { try { o.stop(); } catch {} }); playing = []; }
function play() {
  const pb = S.view && S.view.playback; if (!pb || !pb.notes.length) return;
  stopAll();
  const ac = ctx(), t0 = ac.currentTime + 0.06;
  const sel = [...S.sel];
  const notes = sel.length
    ? pb.notes.filter((_, i) => sel.includes(i))
    : pb.notes;
  const base = notes.length ? notes[0].t : 0;
  for (const n of notes) {
    const osc = ac.createOscillator(), g = ac.createGain();
    osc.type = "triangle";
    osc.frequency.value = 440 * Math.pow(2, (n.p - 69) / 12);
    const on = t0 + (n.t - base), off = on + Math.max(n.d * 0.92, 0.05);
    g.gain.setValueAtTime(0.0001, on);
    g.gain.exponentialRampToValueAtTime(0.22, on + 0.012);
    g.gain.exponentialRampToValueAtTime(0.0001, off);
    osc.connect(g).connect(ac.destination);
    osc.start(on); osc.stop(off + 0.02);
    playing.push(osc);
  }
  status(`playing ${notes.length} notes${sel.length ? " (selection)" : ""}`);
}
