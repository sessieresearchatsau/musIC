/* musIC — the notebook. Cells of set literals and € expressions, each run into
   a set and a score, with the text, the notes and the rows kept in step: select
   characters and the notes they make light up; point at a note and the
   characters that made it light up.

   Row i of a cell is expanded row i of its expression, which is also row i of
   the encoding the server draws -- the same identity the workbench relies on.
   The server says which characters produced each row (ic.sources.rows) and
   where each € sits (ic.sources.blocks); everything here is lookups on those. */

const NB_BARS = 64;              // bars on screen at once
const STORE = "musicic.notebooks.v2";
const OLD_STORE = "musicic.notebook.v1";   // the single notebook, before there were several
// Block hues steer clear of red and gold, which mean "lit" and "selected".
const HUES = [200, 145, 275, 320, 175, 245, 95, 230, 290];
const hue = (i) => `hsl(${HUES[i % HUES.length]} 58% 40%)`;

const DEFAULT_CELLS = [
  "IC(i=0,n=7)[{16, 60+2*i}]",
  "IC(2)[IC(i=1..3)[{8, 60+i}], {16, 67}]",
  "IC(i,1,3)[IC(i)[{16, 60+i}]]",
  "{{16,67},{16,67},{32,69},{32,67},{32,72},{64,71}}",
];

const N = {
  cells: [], active: null, nextId: 1,
  sel: new Set(), anchor: null,       // picked rows (gold), in the active cell
  textLit: new Set(),                 // rows the text selection covers
  hoverLit: null, hoverBlock: null,   // while the pointer is on something
  noteEls: new Map(), rowBar: new Map(),
  grid: 32,                           // carried from an imported piece; no control
  gen: 0,                             // bumped on switching notebooks
  // The target: a piece's sets, which cells are built toward and checked against.
  target: null, tab: "out", tview: null, tnoteEls: new Map(),
  tsel: new Set(), tanchor: null, thover: null,
  cover: new Map(),                   // item -> "ok" | "bad"
  // Levels of concatenation. Level 1 works on the target's sets; each level
  // after works on the parts the level before was written as. An item is
  // { text, rows }: what it is written as, and the target sets it expands to.
  level: 0, items: [], complete: false,
};
// Every notebook, and which one is open. Each keeps its own cells and settings.
const B = { current: null, books: {} };

function status(msg, bad) {
  const n = $("#status"); n.textContent = n.title = msg || "";
  n.style.color = bad ? "var(--accent)" : "var(--dim)";
}
const settings = () => ({
  encoder: $("#encoder").value, numerator: +$("#numer").value || 4,
  denominator: +$("#denom").value || 4, tempo: +$("#tempo").value || 120,
  grid: N.grid || 32,
  order: N.order || null,
});

/* -------------------------------------------------------------- startup */
(async function init() {
  const encs = await (await fetch("/api/encoders")).json();
  N.encs = Object.fromEntries(encs.map((e) => [e.key, e]));
  // A cell's rows are read back as notes, so only the exact encodings apply.
  for (const e of encs.filter((e) => e.invertible && !e.nested)) {
    const o = el("option", null, e.title); o.value = e.key; $("#encoder").append(o);
  }
  $("#encoder").value = "pair";

  $("#addCell").onclick = () => { const c = addCell(""); c.ta.focus(); save(); };
  $("#runAll").onclick = runAll;
  for (const id of ["numer", "denom", "tempo"])
    $("#" + id).onchange = () => { save(); runAll(); };
  // Another encoding has other columns: start it in its own order.
  $("#encoder").onchange = () => { N.order = null; columnMenu(); save(); runAll(); };
  $("#columns").onchange = () => changeOrder($("#columns").value.split(",").map(Number));
  $("#play").onclick = () => play(false);
  $("#playAll").onclick = playAll;
  $("#playSel").onclick = () => play(true);
  $("#stop").onclick = () => { const was = playing.length; stopPlaying(); if (was) status("stopped"); };
  $("#clearSel").onclick = () => { N.sel.clear(); N.anchor = null; light(); };
  $("#copySel").onclick = copySel;
  $("#prevPg").onclick = () => turn(-1);
  $("#nextPg").onclick = () => turn(1);
  $("#book").onchange = () => {
    const v = $("#book").value;
    if (v === "__new") newBook(); else openBook(v);
  };
  $("#renameBook").onclick = renameBook;
  $("#midiIn").onchange = (e) => { const f = e.target.files[0]; e.target.value = ""; if (f) openMidi(f); };
  $("#pieceIn").onchange = () => { const f = $("#pieceIn").value; $("#pieceIn").value = ""; if (f) openPiece(f); };
  document.querySelectorAll(".stabs button").forEach((b) => b.onclick = () => showTab(b.dataset.tab));
  $("#newFromSel").onclick = newCellFromSel;
  $("#tplaySel").onclick = () => {
    const n = N.tview && playNotes(N.tview.playback, N.tsel);
    if (n) status(`playing ${n} target notes`);
  };
  $("#tclear").onclick = () => { N.tsel.clear(); N.tanchor = null; tlight(); };
  $("#checkAll").onclick = checkAll;
  $("#nextLevel").onclick = nextLevel;
  $("#saveBook").onclick = saveBook;
  $("#savedIn").onchange = () => { const f = $("#savedIn").value; $("#savedIn").value = ""; if (f) openSaved(f); };
  $("#exportBtn").onclick = openExport;
  $("#exportDlg").addEventListener("close", () => {
    if ($("#exportDlg").returnValue === "ok") exportPdf(exportChoices(true));
  });
  document.addEventListener("keydown", (e) => {
    if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === "s") { e.preventDefault(); saveBook(); }
  });
  loadSaved();
  splitters();
  loadPieces();
  $("#deleteBook").onclick = deleteBook;
  window.addEventListener("resize", () => {
    N.cells.forEach((c) => size(c));
    if (N.tab === "target" && N.target) drawTarget();
    else if (N.active && N.active.res) drawStage();
  });

  loadBooks();
  // Arriving from the Sheet with a piece: give it a notebook of its own.
  const q = new URLSearchParams(location.search);
  if (q.get("file") || q.get("session")) {
    history.replaceState(null, "", "/notebook");   // a reload must not import twice
    try { await importPiece(q); return; }
    catch (err) { status("could not bring the piece in: " + err.message, true); }
  }
  await openBook(B.current);
})();

/* ------------------------------------------------------------ notebooks */
function loadBooks() {
  try {
    const j = JSON.parse(localStorage.getItem(STORE) || "null");
    if (j && j.books) Object.assign(B, j);
  } catch { /* a private window keeps nothing; the page still works */ }
  if (!Object.keys(B.books).length) {
    let old = null;
    try { old = JSON.parse(localStorage.getItem(OLD_STORE) || "null"); } catch {}
    B.books.nb1 = { name: "My notebook",
      cells: old && old.cells && old.cells.length ? old.cells : DEFAULT_CELLS,
      encoder: (old && old.encoder) || "pair", numerator: (old && old.numerator) || 4,
      denominator: (old && old.denominator) || 4, tempo: (old && old.tempo) || 120, grid: 32 };
    B.current = "nb1";
  }
  if (!B.books[B.current]) B.current = Object.keys(B.books)[0];
}

function save() {
  const b = B.books[B.current];
  if (b) {
    Object.assign(b, settings());
    const lv = b.levels[b.level || 0];
    lv.cells = N.cells.map((c) => c.ta.value);
    lv.claims = N.cells.map((c) => c.claim || null);
  }
  try { localStorage.setItem(STORE, JSON.stringify(B)); } catch {}
}

function bookMenu() {
  const sel = $("#book"); sel.innerHTML = "";
  for (const [id, b] of Object.entries(B.books)) {
    const o = el("option", null, b.name); o.value = id; sel.append(o);
  }
  const o = el("option", null, "+ New notebook"); o.value = "__new"; sel.append(o);
  sel.value = B.current;
}

async function openBook(id) {
  const gen = ++N.gen;
  B.current = id;
  const b = B.books[id];
  if ([...$("#encoder").options].some((o) => o.value === b.encoder)) $("#encoder").value = b.encoder;
  $("#numer").value = b.numerator || 4;
  $("#denom").value = b.denominator || 4;
  $("#tempo").value = b.tempo || 120;
  N.grid = b.grid || 32;
  N.order = b.order || null;
  columnMenu();
  // Notebooks from before levels held one list of cells: that is level 1.
  if (!b.levels) {
    b.levels = [{ cells: b.cells || [""], claims: b.claims || [] }];
    b.level = 0; delete b.cells; delete b.claims;
  }
  N.target = b.target || null;
  loadLevel(b);
  N.tview = null;
  document.body.classList.toggle("has-target", !!N.target);
  // A target's rows are in one encoding; cells must be read the same way.
  $("#encoder").disabled = !!N.target;
  $("#encoder").title = N.target ? "set by the target piece" : "";
  $("#stabs").hidden = !N.target;
  N.tab = N.target ? "target" : "out";
  showTab(N.tab, true);
  bookMenu(); save();
  document.title = `${b.name} — musIC notebook`;
  const loading = N.target ? loadTarget(gen) : null;
  await runAll(gen);
  if (loading) await loading;
  if (gen === N.gen && N.cells.length && !N.active) setActive(N.cells[0]);
  if (gen === N.gen) coverage();
}

function newBook() {
  let k = Object.keys(B.books).length + 1;
  while (Object.values(B.books).some((b) => b.name === `Notebook ${k}`)) k++;
  const id = "nb" + Date.now();
  B.books[id] = { name: `Notebook ${k}`, cells: [""], ...settings() };
  openBook(id).then(() => N.cells[0] && N.cells[0].ta.focus());
}

function renameBook() {
  const b = B.books[B.current];
  const name = prompt("Name this notebook", b.name);
  if (name && name.trim()) { b.name = name.trim(); bookMenu(); save(); }
}

function deleteBook() {
  const b = B.books[B.current];
  if (!confirm(`Delete the notebook “${b.name}” and its cells? This cannot be undone.`)) return;
  delete B.books[B.current];
  if (!Object.keys(B.books).length)
    B.books.nb1 = { name: "My notebook", cells: [""], ...settings() };
  openBook(Object.keys(B.books)[0]);
}

/* A piece from the Sheet (or a library file) as a notebook: its whole set in
   one cell, a bar to a line, so a bar can be selected to light it up and any
   run of bars rewritten in place as a €. A library piece reopens the notebook
   made from it before, keeping whatever was written there. */
async function importPiece(q) {
  const exact = [...$("#encoder").options].map((o) => o.value);
  const reduction = q.get("reduction") || "top";
  let session = q.get("session"), name = "", key = null;
  let encoder = q.get("encoder");
  const track = +q.get("track") || 0;
  if (q.get("file")) {
    const j = await api("/api/library/open", { file: q.get("file") });
    session = j.session; name = j.setfile.name;
    encoder = encoder || j.setfile.encoder;
  }
  // Lossy and grouped encodings cannot be read back as notes.
  if (!exact.includes(encoder)) encoder = "pair";
  if (q.get("file")) key = `file:${q.get("file")}:${encoder}:${reduction}`;
  if (key && B.books[key]) return openBook(key);

  status("bringing the piece in…");
  const v = await api("/api/view", { session, track, encoder, reduction,
    max_measures: 100000 });              // every bar, in one page
  const rowBar = new Map();
  for (const m of v.score.measures)
    for (const n of m.notes)
      for (const r of n.rows || []) if (!rowBar.has(r)) rowBar.set(r, m.index);
  const lines = [];
  v.encoding.rows.forEach((r, i) => {
    const bar = rowBar.has(i) ? rowBar.get(i) : lines.length - 1;
    while (lines.length <= bar) lines.push([]);
    lines[Math.max(bar, 0)].push("{" + r.join(",") + "}");
  });
  const text = "{" + lines.filter((l) => l.length).map((l) => l.join(",")).join(",\n ") + "}";
  const id = key || "nb" + Date.now();
  B.books[id] = {
    name: name || v.track.name || "Imported piece", cells: [text], encoder,
    numerator: v.track.numerator, denominator: v.track.denominator,
    tempo: Math.round(v.track.tempo || 120), grid: v.track.grid || 32,
    claims: [v.encoding.rows.map((_, i) => i)],
    target: targetFrom(v, name || v.track.name, encoder),
  };
  await openBook(id);
  status(`${B.books[id].name}: ${v.encoding.rows.length} rows, a bar to a line`);
}

/* ---------------------------------------------------------------- cells */
function addCell(text, after) {
  const c = { id: N.nextId++, res: null, err: null, dirty: false };
  c.root = el("div", "cell");
  const lab = el("span", "lbl"); c.inLbl = lab;
  const ed = el("div", "ed");
  c.mirror = el("div", "mirror"); c.mirror.setAttribute("aria-hidden", "true");
  c.ta = el("textarea"); c.ta.value = text || ""; c.ta.spellcheck = false; c.ta.rows = 1;
  c.ta.setAttribute("aria-label", "cell input");
  // Cells are code: keep writing assistants (Grammarly) off them.
  for (const a of ["data-gramm", "data-gramm_editor", "data-enable-grammarly"]) c.ta.setAttribute(a, "false");
  c.ta.placeholder = "IC(i,0,3)[{16, 60+i}]";
  const run = el("button", "run", "▶"); run.title = "Run (Shift+Enter)";
  run.onclick = () => runCell(c);
  ed.append(c.mirror, c.ta, run);

  c.out = el("div", "out");
  c.outLbl = el("span", "lbl");
  c.res_el = el("div", "res");
  c.out.append(c.outLbl, c.res_el); c.out.hidden = true;

  const tools = el("div", "tools");
  const up = el("button", null, "↑"); up.title = "Move up";
  const down = el("button", null, "↓"); down.title = "Move down";
  const ins = el("button", null, "+ below");
  const del = el("button", null, "Delete");
  const chk = el("button", "needs-target", "✓ Check");
  chk.title = "Expand this cell and compare it with the target sets it stands for";
  chk.onclick = () => checkCell(c);
  up.onclick = () => move(c, -1); down.onclick = () => move(c, 1);
  ins.onclick = () => { const n = addCell("", c); n.ta.focus(); save(); };
  del.onclick = () => removeCell(c);
  tools.append(chk, up, down, ins, del);

  c.root.append(lab, ed, c.out, tools);
  const i = after ? N.cells.indexOf(after) + 1 : N.cells.length;
  N.cells.splice(i, 0, c);
  const host = $("#cells");
  host.insertBefore(c.root, host.children[i] || null);

  c.ta.addEventListener("input", () => {
    c.dirty = !!c.res || !!c.err; size(c); paint(c); drawOut(c); save();
  });
  c.ta.addEventListener("focus", () => setActive(c));
  c.ta.addEventListener("keydown", (e) => {
    if (e.key === "Enter" && (e.shiftKey || e.metaKey || e.ctrlKey)) {
      e.preventDefault();
      runCell(c).then(() => {
        if (!e.shiftKey) return;          // Shift+Enter moves on, as in Mathematica
        const k = N.cells.indexOf(c);
        const next = N.cells[k + 1] || addCell("");
        next.ta.focus(); save();
      });
    }
  });
  // Any change of the text selection re-lights the music.
  for (const ev of ["select", "keyup", "mouseup"])
    c.ta.addEventListener(ev, () => fromText(c));
  // The bracket pair follows the cursor, however it moves.
  for (const ev of ["keyup", "mouseup", "select", "focus", "blur", "input"])
    c.ta.addEventListener(ev, () => requestAnimationFrame(() => paint(c)));

  renumber(); size(c); paint(c);
  return c;
}

function removeCell(c) {
  const i = N.cells.indexOf(c);
  N.cells.splice(i, 1); c.root.remove();
  if (!N.cells.length) addCell("");
  if (N.active === c) setActive(N.cells[Math.min(i, N.cells.length - 1)]);
  renumber(); save();
}

function move(c, d) {
  const i = N.cells.indexOf(c), j = i + d;
  if (j < 0 || j >= N.cells.length) return;
  N.cells.splice(i, 1); N.cells.splice(j, 0, c);
  const host = $("#cells");
  host.insertBefore(c.root, host.children[j + (d > 0 ? 1 : 0)] || null);
  renumber(); save();
}

function renumber() {
  N.cells.forEach((c, i) => {
    c.n = i + 1;
    c.inLbl.textContent = `In[${c.n}]:=`;
    c.outLbl.textContent = `Out[${c.n}]=`;
  });
  if (N.active) $("#ptitle").innerHTML = `Out[${N.active.n}]`;
}

function size(c) {
  c.ta.style.height = "auto";
  c.ta.style.height = c.ta.scrollHeight + 2 + "px";
}

/* ------------------------------------------------------------------ run */
async function runAll(gen) {
  gen = typeof gen === "number" ? gen : N.gen;
  for (const c of [...N.cells]) {
    if (gen !== N.gen) return;          // switched notebooks meanwhile
    if (c.ta.value.trim()) await runCell(c, true);
  }
  if (gen === N.gen && N.active) drawStage();
}

async function runCell(c, quiet) {
  const text = c.ta.value.trim() ? c.ta.value : "";
  if (!text) { c.res = null; c.err = null; c.dirty = false; drawOut(c); paint(c); return; }
  const s = settings();
  try {
    const j = await api("/api/ic", { text, ...s });
    if (!N.cells.includes(c)) return;   // the cell went with a notebook switch
    const view = await api("/api/view", { session: j.session, encoder: s.encoder, order: s.order,
      reduction: "full", max_measures: NB_BARS });
    c.res = { text, session: j.session, ic: j.ic, view, page: 0, rows: j.rows };
    c.err = null;
    index(c);
    c.check = N.target && c.claim ? check(c) : null;
    if (!quiet) status(`In[${c.n}] → ${j.ic.rows} rows`);
  } catch (err) {
    c.res = null; c.err = err.message; c.check = null;
    if (!quiet) status(`In[${c.n}]: ${err.message}`, true);
  }
  c.dirty = false;
  drawOut(c); paint(c);
  if (N.target && !quiet) coverage();
  if (!quiet || N.active === c) {
    if (N.active !== c) setActive(c); else { resetLight(); drawStage(); }
  }
}

/* Derive the lookups a cell's result needs. */
function index(c) {
  const ic = c.res.ic, src = ic.sources || { rows: [], blocks: [] };
  c.blocks = src.blocks;                       // parse order: the colour order
  c.color = new Map(c.blocks.map((b, i) => [b.id, i]));
  c.rowSrc = src.rows;
  c.spans = ic.spans || [];
  c.rowsOf = new Map();                        // block id -> rows, all copies
  for (const s of c.spans) {
    if (!c.rowsOf.has(s.id)) c.rowsOf.set(s.id, []);
    for (let r = s.start; r <= s.end; r++) c.rowsOf.get(s.id).push(r);
  }
  // Each row takes the colour of the deepest block containing it.
  c.blockOfRow = new Map();
  for (const s of [...c.spans].sort((a, b) => a.depth - b.depth))
    for (let r = s.start; r <= s.end; r++) c.blockOfRow.set(r, s.id);
}

/* ------------------------------------------------------------------ out */
function drawOut(c) {
  const box = c.res_el; box.innerHTML = "";
  c.out.hidden = !c.res && !c.err && !(N.target && c.claim);
  if (N.target && c.claim) box.append(claimLine(c));
  if (c.err) { box.append(el("div", "err", c.err)); return; }
  if (!c.res) return;
  const v = c.res.view, ic = c.res.ic;
  const line = el("div");
  line.innerHTML = `<b>${ic.rows}</b> rows · <b>${v.score.total_measures}</b> bars`;
  if (c.dirty) line.append(el("span", "stale", "  · edited — Shift+Enter to run again"));
  box.append(line);

  // One chip per €: what it is, how many copies of it the expansion holds.
  const chips = el("div", "chips");
  for (const b of c.blocks) {
    const rows = c.rowsOf.get(b.id) || [];
    const copies = c.spans.filter((s) => s.id === b.id).length;
    const chip = el("span", "chip"); chip.dataset.block = b.id;
    const sw = el("span", "sw"); sw.style.background = hue(c.color.get(b.id));
    chip.append(sw, document.createTextNode(b.label));
    chip.append(el("small", null, copies === 0 ? "vanishes"
      : copies > 1 ? `×${copies} · ${rows.length} rows` : `${rows.length} rows`));
    chip.title = "Hover to light it up, click to select and play it";
    chip.addEventListener("mouseenter", () => {
      if (N.active !== c) return;
      N.hoverLit = new Set(rows); N.hoverBlock = b.id; light();
    });
    chip.addEventListener("mouseleave", () => { N.hoverLit = null; N.hoverBlock = null; light(); });
    chip.addEventListener("click", () => {
      if (N.active !== c) setActive(c);
      N.sel = new Set(rows); N.anchor = null; light();
      if (rows.length) play(true);
    });
    chips.append(chip);
  }
  if (c.blocks.length) box.append(chips);

  const det = el("details");
  det.append(el("summary", null, "expanded set"), el("code", null, ic.expanded));
  box.append(det);
}

/* ---------------------------------------------------------------- mirror */
/* Draw the cell's text with its € blocks coloured and the rows in play marked.
   Spans may nest (a lit row inside a coloured block), so the text is cut at
   every boundary and each piece takes all the marks covering it. */
function paint(c) {
  const text = c.ta.value;
  const marks = [];
  if (c.res && !c.dirty && c.res.text === text) {
    for (const b of c.blocks) {
      if (!b.src) continue;
      const [a, body, end] = b.src, color = hue(c.color.get(b.id));
      marks.push({ a, b: body, color }, { a: end - 1, b: end, color });
    }
    if (c === N.active) {
      const lit = N.hoverLit || N.textLit;
      for (const r of N.sel) if (c.rowSrc[r]) marks.push({ a: c.rowSrc[r][0], b: c.rowSrc[r][1], cls: "sel" });
      for (const r of lit) if (c.rowSrc[r]) marks.push({ a: c.rowSrc[r][0], b: c.rowSrc[r][1], cls: "lit" });
      const hb = N.hoverBlock && c.blocks.find((b) => b.id === N.hoverBlock);
      if (hb && hb.src) marks.push({ a: hb.src[0], b: hb.src[1], cls: "lit" },
                                   { a: hb.src[2] - 1, b: hb.src[2], cls: "lit" });
    }
  }
  // Brackets, as an editor shows them: the pair at the cursor, and any
  // bracket with no partner -- these show while typing, before a run.
  const br = brackets(text);
  for (const k of br.lone) marks.push({ a: k, b: k + 1, cls: "lone" });
  if (document.activeElement === c.ta && c.ta.selectionStart === c.ta.selectionEnd) {
    const at = c.ta.selectionStart;
    // The bracket just before the cursor, else the one just after it.
    const k = br.mate.has(at - 1) ? at - 1 : br.mate.has(at) ? at : null;
    if (k != null) {
      marks.push({ a: k, b: k + 1, cls: "pair" });
      const m = br.mate.get(k);
      if (m != null) marks.push({ a: m, b: m + 1, cls: "pair" });
    }
  }
  const cuts = new Set([0, text.length]);
  for (const m of marks) { cuts.add(m.a); cuts.add(m.b); }
  const pts = [...cuts].filter((x) => x >= 0 && x <= text.length).sort((a, b) => a - b);
  let html = "";
  for (let k = 0; k < pts.length - 1; k++) {
    const a = pts[k], b = pts[k + 1];
    const on = marks.filter((m) => m.a <= a && m.b >= b);
    const piece = esc(text.slice(a, b));
    if (!on.length) { html += piece; continue; }
    const cls = [];
    if (on.some((m) => m.cls === "sel")) cls.push("sel");
    if (on.some((m) => m.cls === "lit")) cls.push("lit");
    if (on.some((m) => m.cls === "pair")) cls.push("pair");
    if (on.some((m) => m.cls === "lone")) cls.push("lone");
    const col = on.filter((m) => m.color).pop();
    if (col) cls.push("h");
    html += `<span class="${cls.join(" ")}"${col ? ` style="color:${col.color}"` : ""}>${piece}</span>`;
  }
  // A trailing newline needs a character after it to take up its line.
  c.mirror.innerHTML = html + (text.endsWith("\n") ? " " : "");
}

/* Match brackets the way the parser reads them: (), [] and {} nest, and a
   closer must close the most recent opener. `mate` maps each bracket's
   position to its partner's (null for one with none); `lone` lists those. */
function brackets(text) {
  const OPEN = { "(": ")", "[": "]", "{": "}" }, CLOSE = { ")": "(", "]": "[", "}": "{" };
  const mate = new Map(), lone = [], stack = [];
  for (let k = 0; k < text.length; k++) {
    const ch = text[k];
    if (OPEN[ch]) { stack.push(k); mate.set(k, null); }
    else if (CLOSE[ch]) {
      const top = stack[stack.length - 1];
      if (top != null && text[top] === CLOSE[ch]) {
        stack.pop(); mate.set(top, k); mate.set(k, top);
      } else {
        mate.set(k, null); lone.push(k);
      }
    }
  }
  lone.push(...stack);
  return { mate, lone };
}

/* -------------------------------------------------------- text -> music */
function fromText(c) {
  if (N.active !== c) setActive(c);
  if (!c.res || c.dirty) return;
  const a = c.ta.selectionStart, b = c.ta.selectionEnd;
  const rows = new Set();
  const src = c.rowSrc;
  src.forEach((r, i) => {
    if (!r) return;
    const hit = a === b ? a >= r[0] && a <= r[1] : r[0] < b && r[1] > a;
    if (hit) rows.add(i);
  });
  // A caret on a € head, or a selection that takes one in, means the whole block.
  for (const blk of c.blocks) {
    if (!blk.src) continue;
    const [s, body, end] = blk.src;
    const onHead = a === b ? (a >= s && a <= body) || a === end || a === end - 1
                           : s < b && body > a;
    if (onHead) for (const r of c.rowsOf.get(blk.id) || []) rows.add(r);
  }
  N.textLit = rows;
  light();
  if (rows.size) {
    const first = Math.min(...rows);
    const onPage = N.rowBar.has(first);
    if (!onPage) status(`row ${first + 1} is on another page`);
  }
}

/* ---------------------------------------------------------------- stage */
function setActive(c) {
  if (N.active === c) return;
  if (N.active) { N.active.root.classList.remove("active"); const p = N.active; N.active = c; paint(p); }
  N.active = c;
  c.root.classList.add("active");
  resetLight();
  drawStage();
}

function resetLight() {
  N.sel = new Set(); N.anchor = null; N.textLit = new Set();
  N.hoverLit = null; N.hoverBlock = null;
}

function drawStage() {
  const c = N.active;
  $("#outTab").textContent = c ? `Out[${c.n}]` : "Out";
  if (N.tab !== "out") return;          // drawn when its tab is shown
  $("#ptitle").innerHTML = c ? `Out[${c.n}]` : "Out";
  const has = c && c.res;
  $("#plinks").hidden = !has;
  if (!has) {
    $("#pmeta").textContent = "";
    $("#score").innerHTML = `<div class="empty">${c && c.err
      ? "This cell did not run — see the message under it."
      : "Run the cell (Shift+Enter) to see its music here."}</div>`;
    $("#folio").hidden = true; $("#bars").innerHTML = "";
    $("#setTitle").textContent = "Set"; $("#setMeta").textContent = "";
    N.noteEls = new Map(); N.rowBar = new Map(); light();
    return;
  }
  const v = c.res.view, sc = v.score;
  $("#ptitle").innerHTML = `Out[${c.n}] <small>${esc(v.encoding.title)}</small>`;
  $("#pmeta").textContent = [`♩ = ${Math.round(sc.tempo || 120)}`,
    sc.key_display, `${sc.numerator}/${sc.denominator}`].filter(Boolean).join(" · ");
  const q = new URLSearchParams({ session: c.res.session, encoder: v.encoding.encoder,
    reduction: "full" });
  $("#toSheet").href = "/sheet?" + q;
  $("#toBench").href = "/?" + q;

  const out = engrave($("#score"), sc, {
    barNumbers: true, lineH: 124,
    onNote: (g, sn) => {
      g.addEventListener("mouseenter", () => { N.hoverLit = new Set(sn._rows); light(sn._rows[0]); });
      g.addEventListener("mouseleave", () => { N.hoverLit = null; light(); });
      g.addEventListener("click", (e) => pick(sn._rows[0], e.shiftKey, sn._rows));
    },
  });
  N.noteEls = out.noteEls;
  const pages = sc.pages || 1;
  $("#folio").hidden = pages < 2;
  $("#pageNo").textContent = `page ${(sc.page || 0) + 1} of ${pages}`;
  $("#prevPg").disabled = (sc.page || 0) <= 0;
  $("#nextPg").disabled = (sc.page || 0) >= pages - 1;
  drawRows();
  light();
}

async function turn(d) {
  const c = N.active; if (!c || !c.res) return;
  try {
    c.res.view = await api("/api/view", { session: c.res.session, order: N.order,
      encoder: c.res.view.encoding.encoder, reduction: "full",
      max_measures: NB_BARS, page: (c.res.view.score.page || 0) + d });
    drawStage();
  } catch (err) { status(err.message, true); }
}

/* The rows under the bar their note sits in, edged in their block's colour. */
function drawRows() {
  const c = N.active, v = c.res.view, enc = v.encoding, host = $("#bars");
  host.innerHTML = "";
  $("#setTitle").textContent = enc.title;
  N.rowBar = new Map();
  for (const m of v.score.measures)
    for (const n of m.notes)
      for (const r of n.rows || []) if (!N.rowBar.has(r)) N.rowBar.set(r, m.index);
  const onPage = enc.rows.map((_, i) => i).filter((i) => N.rowBar.has(i));
  $("#setMeta").textContent = onPage.length === enc.rows.length
    ? `${enc.rows.length} rows`
    : `rows ${onPage[0] + 1}–${onPage[onPage.length - 1] + 1} of ${enc.rows.length}`;
  let cur = null, curBar = -1;
  for (const i of onPage) {
    const bar = N.rowBar.get(i);
    if (bar !== curBar) {
      curBar = bar;
      const b = el("div", "bar");
      b.append(el("span", "num", `bar ${bar + 1}`));
      cur = el("div", "rows"); b.append(cur); host.append(b);
    }
    const chip = el("span", "row"); chip.dataset.row = i;
    const blk = c.blockOfRow.get(i);
    if (blk != null) chip.style.borderLeftColor = hue(c.color.get(blk));
    chip.append(el("i", null, i + 1), document.createTextNode("{" + enc.rows[i].join(",") + "}"));
    chip.addEventListener("mouseenter", () => { N.hoverLit = new Set([i]); light(i, true); });
    chip.addEventListener("mouseleave", () => { N.hoverLit = null; light(); });
    chip.addEventListener("click", (e) => pick(i, e.shiftKey));
    cur.append(chip);
  }
}

/* ------------------------------------------------------------- lighting */
/* Paint the three views from the state: lit rows red, picked rows gold, in the
   score, the row list and the cell's text alike. */
function light(focusRow, fromList) {
  $("#outView").querySelectorAll(".hot").forEach((n) => n.classList.remove("hot"));
  $("#outView").querySelectorAll(".picked").forEach((n) => n.classList.remove("picked"));
  document.querySelectorAll("#bars .row.sel").forEach((n) => n.classList.remove("sel"));
  document.querySelectorAll(".chip.on").forEach((n) => n.classList.remove("on"));
  const c = N.active;
  const lit = N.hoverLit || N.textLit;
  for (const r of N.sel) {
    (N.noteEls.get(r) || []).forEach((g) => g.classList.add("picked"));
    const ch = document.querySelector(`#bars .row[data-row="${r}"]`);
    if (ch) ch.classList.add("sel");
  }
  let firstChip = null;
  for (const r of lit) {
    (N.noteEls.get(r) || []).forEach((g) => g.classList.add("hot"));
    const ch = document.querySelector(`#bars .row[data-row="${r}"]`);
    if (ch) { ch.classList.add("hot"); firstChip = firstChip || ch; }
  }
  // Follow along in the list, unless the pointer is in the list itself.
  if (firstChip && !fromList) firstChip.scrollIntoView({ block: "nearest" });
  if (c && c.res) {
    for (const [id, rows] of c.rowsOf)
      if (rows.length && rows.every((r) => N.sel.has(r)))
        c.root.querySelector(`.chip[data-block="${id}"]`)?.classList.add("on");
    paint(c);
  }
  readout(focusRow, lit);
  const n = N.sel.size;
  $("#playSel").disabled = $("#clearSel").disabled = !n;
  $("#selInfo").textContent = n
    ? `${n} selected — rows ${[...N.sel].sort((a, b) => a - b).map((i) => i + 1).join(", ")}`.slice(0, 150)
    : "Click notes, rows or € blocks to select; shift-click for a range.";
}

function readout(row, lit) {
  const c = N.active, box = $("#readout");
  if (!c || !c.res) { box.textContent = "Select text in a cell, or hover a note or a row."; return; }
  if (row == null && lit.size === 1) row = [...lit][0];
  if (row == null) {
    box.innerHTML = lit.size ? `<b>${lit.size} rows</b> lit` + (N.hoverBlock
      ? ` — ${esc(c.blocks.find((b) => b.id === N.hoverBlock)?.label || "")}` : "")
      : "Select text in a cell, or hover a note or a row.";
    return;
  }
  const enc = c.res.view.encoding, vals = enc.rows[row];
  const desc = vals ? enc.columns.map((k, i) => `${k} ${vals[i]}`).join(" · ") : "";
  let note = "";
  for (const m of c.res.view.score.measures) for (const n of m.notes)
    if (!note && (n.rows || []).includes(row))
      note = `${n.label} · ${n.code}${".".repeat(n.dots)} · bar ${m.index + 1}`;
  const blk = c.blockOfRow.get(row);
  const from = blk != null ? c.blocks.find((b) => b.id === blk) : null;
  box.innerHTML = `<b>row ${row + 1}</b> &nbsp;${esc(desc)}`
    + (note ? ` → <b>${esc(note)}</b>` : "")
    + (from ? ` <span style="color:${hue(c.color.get(blk))}">in ${esc(from.label)}</span>` : "");
}

/* Click picks or unpicks a row; shift-click picks everything from the last
   pick to here, the way a range is taken in a list. */
function pick(row, range, rows) {
  if (range && N.anchor != null) {
    const [a, b] = [Math.min(N.anchor, row), Math.max(N.anchor, row)];
    for (let r = a; r <= b; r++) N.sel.add(r);
  } else {
    const all = rows && rows.length ? rows : [row];
    const on = all.every((r) => N.sel.has(r));
    for (const r of all) on ? N.sel.delete(r) : N.sel.add(r);
    N.anchor = row;
  }
  light(row);
}

/* ----------------------------------------------------------- play, copy */
function play(selectionOnly) {
  const c = N.active; if (!c || !c.res) return;
  const n = playNotes(c.res.view.playback, selectionOnly ? N.sel : null);
  if (n) status(`playing ${n} notes${selectionOnly ? " (selection)" : ""}`);
}

async function copySel() {
  const c = N.active; if (!c || !c.res) return;
  const enc = c.res.view.encoding;
  const text = N.sel.size
    ? "{" + [...N.sel].sort((a, b) => a - b).map((i) => "{" + enc.rows[i].join(",") + "}").join(",") + "}"
    : enc.mathematica;
  try { await navigator.clipboard.writeText(text);
    status(N.sel.size ? "copied the selection" : "copied the set"); }
  catch { status("copy blocked by the browser", true); }
}

/* =============================================================== target */
/* A notebook can work toward a piece. Its sets are the target: select some,
   make a cell of them, rewrite that cell as concatenations, and the notebook
   expands what you wrote and checks it against the sets it came from. */

function targetFrom(v, name, encoder) {
  const bars = v.encoding.rows.map(() => null);
  for (const m of v.score.measures)
    for (const n of m.notes)
      for (const r of n.rows || []) if (bars[r] == null) bars[r] = m.index;
  return { name: name || v.track.name, encoder, rows: v.encoding.rows, bars,
           numerator: v.track.numerator, denominator: v.track.denominator,
           tempo: Math.round(v.track.tempo || 120), grid: v.track.grid || 32 };
}

async function loadPieces() {
  try {
    const j = await fetch("/api/library").then((r) => r.json());
    for (const f of j.files) {
      if (f.error) continue;
      const o = el("option", null, f.name); o.value = f.file; $("#pieceIn").append(o);
    }
  } catch { /* the library is optional */ }
}

async function openMidi(f) {
  status("reading " + f.name + "…");
  const fd = new FormData(); fd.append("file", f); fd.append("grid", "32");
  try {
    const j = await api("/api/midi", fd, true);
    if (j.tracks.length === 1) return startFrom(j.session, 0, f.name.replace(/\.midi?$/i, ""));
    // Several tracks: say which one to work toward.
    const pick = $("#trackPick"); pick.innerHTML = ""; pick.hidden = false;
    pick.append(el("option", null, `${j.tracks.length} tracks — choose one…`));
    j.tracks.forEach((t, i) => {
      const o = el("option", null, `${t.name} — ${t.notes} notes`); o.value = i; pick.append(o);
    });
    pick.focus();
    pick.onchange = () => {
      if (pick.value === "") return;
      pick.hidden = true;
      const t = j.tracks[+pick.value];
      startFrom(j.session, +pick.value, `${f.name.replace(/\.midi?$/i, "")} — ${t.name}`);
    };
    status(`${f.name}: choose the track to work toward`);
  } catch (err) { status(err.message, true); }
}

async function openPiece(file) {
  status("opening " + file + "…");
  try {
    const j = await api("/api/library/open", { file });
    await startFrom(j.session, 0, j.setfile.name, j.setfile.encoder);
  } catch (err) { status(err.message, true); }
}

/* A new notebook whose target is this track's sets, with one empty cell. The
   melody is read as one line unless the rows keep start times (triple). */
async function startFrom(session, track, name, encoder) {
  const exact = [...$("#encoder").options].map((o) => o.value);
  encoder = exact.includes(encoder) ? encoder : exact.includes($("#encoder").value)
    ? $("#encoder").value : "pair";
  const reduction = encoder === "triple" ? "full" : "top";
  try {
    const v = await api("/api/view", { session, track, encoder, reduction,
      max_measures: 100000 });
    const id = "nb" + Date.now();
    B.books[id] = { name, cells: [""], claims: [null], encoder,
      numerator: v.track.numerator, denominator: v.track.denominator,
      tempo: Math.round(v.track.tempo || 120), grid: v.track.grid || 32,
      target: targetFrom(v, name, encoder) };
    await openBook(id);
    status(`${name}: ${v.encoding.rows.length} sets to work toward — select some and make a cell`);
  } catch (err) { status(err.message, true); }
}

/* The target is stored as rows, not as a server session, so it outlives a
   restart; to draw it, it is simply pasted back in as a set. */
async function loadTarget(gen) {
  const t = N.target;
  try {
    const lit = "{" + t.rows.map((r) => "{" + r.join(",") + "}").join(",") + "}";
    const j = await api("/api/set", { text: lit, encoder: t.encoder, grid: t.grid || 32, order: N.order,
      tempo: t.tempo || 120, numerator: t.numerator || 4, denominator: t.denominator || 4,
      name: t.name });
    const v = await api("/api/view", { session: j.session, encoder: t.encoder, order: N.order,
      reduction: "full", max_measures: 100000 });
    if (gen !== N.gen) return;
    N.tview = v;
    if (N.tab === "target") drawTarget();
  } catch (err) { status("could not draw the target: " + err.message, true); }
}

function showTab(tab, quiet) {
  N.tab = tab;
  document.querySelectorAll(".stabs button").forEach((b) =>
    b.setAttribute("aria-selected", b.dataset.tab === tab));
  $("#targetView").hidden = tab !== "target";
  $("#outView").hidden = tab !== "out";
  if (quiet) return;
  if (tab === "target") drawTarget(); else drawStage();
}

/* ---------------------------------------------------------------- levels */
/* Put the notebook's current level on screen: its cells and its items. */
function loadLevel(b) {
  const lv = b.levels[b.level || 0];
  N.level = b.level || 0;
  N.cells.forEach((c) => c.root.remove());
  N.cells = []; N.active = null;
  (lv.cells && lv.cells.length ? lv.cells : [""]).forEach((t, i) => {
    addCell(t).claim = (lv.claims && lv.claims[i]) || null;
  });
  N.items = N.target ? (lv.items || N.target.rows.map((r, i) =>
    ({ text: "{" + r.join(",") + "}", rows: [i] }))) : [];
  N.tsel = new Set(); N.tanchor = null; N.thover = null;
  N.cover = new Map(); N.complete = false;
  $("#tinfo").textContent = "Green: written and matching. Red: written, but different.";
  drawLevels(b);
}

const noun = (n) => (N.level ? (n === 1 ? "item" : "items") : (n === 1 ? "set" : "sets"));

function drawLevels(b) {
  const host = $("#levels");
  host.hidden = !N.target;
  host.innerHTML = "";
  if (!N.target) return;
  b.levels.forEach((lv, k) => {
    const t = el("button", null, `Level ${k + 1}`);
    t.setAttribute("aria-selected", k === N.level);
    t.title = k === 0 ? `${N.target.rows.length} sets of the piece`
                      : `${(lv.items || []).length} items: level ${k} as it was written`;
    t.onclick = () => goLevel(k);
    host.append(t);
  });
  host.append(el("span", "sub", N.level
    ? `concatenating level ${N.level}'s parts — ${N.items.length} items`
    : `concatenating the piece's sets — ${N.items.length} sets`));
}

async function goLevel(k) {
  const b = B.books[B.current];
  if (k === N.level || !b.levels[k]) return;
  save();
  const gen = ++N.gen;
  b.level = k;
  loadLevel(b);
  save();
  drawTarget();
  await runAll(gen);
  if (gen === N.gen && N.cells.length && !N.active) setActive(N.cells[0]);
  if (gen === N.gen) coverage();
}

/* Once every item of this level is written, checks out and is used exactly
   once, the parts it was written as become the items of the next level. */
async function nextLevel() {
  if (!N.complete) return;
  const b = B.books[B.current];
  // Cells in the order of the items they stand for.
  const cells = N.cells.filter((c) => c.claim && c.claim.length)
    .sort((x, y) => Math.min(...x.claim) - Math.min(...y.claim));
  const items = [];
  for (const c of cells) {
    const parts = c.res && c.res.ic.sources && c.res.ic.sources.parts;
    if (!parts || !parts.length || parts.some((p) => !p.src)) {
      status(`In[${c.n}] could not be split into parts; run it again`, true); return;
    }
    const rows = c.claim.flatMap((i) => N.items[i].rows);   // in expansion order
    let at = 0;
    for (const p of parts) {
      if (!p.rows) continue;                   // a part that vanishes, like €0
      items.push({ text: c.res.text.slice(p.src[0], p.src[1]).replace(/\s+/g, " ").trim(),
                   rows: rows.slice(at, at + p.rows) });
      at += p.rows;
    }
  }
  const k = N.level + 1;
  if (b.levels[k] && b.levels[k].cells.some((t) => t.trim()) &&
      !confirm(`Level ${k + 1} already has cells. Start it again from this level?`)) return;
  b.levels.length = k;                          // later levels were built on the old one
  b.levels.push({ cells: [""], claims: [null], items });
  save();
  await goLevel(k);
  status(`Level ${k + 1}: level ${k} as it was written, ${items.length} items — concatenate them again`);
}

/* ---------------------------------------------------------------- target */
function drawTarget() {
  const t = N.target; if (!t || N.tab !== "target") return;
  $("#ttitle").textContent = t.name || "Target";
  $("#tmeta").textContent = [`♩ = ${t.tempo || 120}`, `${t.numerator}/${t.denominator}`,
    `${t.rows.length} sets`, `each set is ${columnsText(N.order)}`].join(" · ");
  // Target row -> the item at this level that contains it.
  N.itemOf = new Map();
  N.items.forEach((it, k) => it.rows.forEach((r) => N.itemOf.set(r, k)));
  if (N.tview) {
    const out = engrave($("#tscore"), N.tview.score, {
      barNumbers: true, lineH: 118,
      onNote: (g, sn) => {
        const items = () => [...new Set(sn._rows.map((r) => N.itemOf.get(r)))].filter((x) => x != null);
        g.addEventListener("mouseenter", () => { N.thover = items(); tlight(); });
        g.addEventListener("mouseleave", () => { N.thover = null; tlight(); });
        g.addEventListener("click", (e) => { const it = items(); if (it.length) tpick(it[0], e.shiftKey, it); });
      },
    });
    N.tnoteEls = out.noteEls;
  } else {
    $("#tscore").innerHTML = `<div class="empty">drawing…</div>`;
  }
  const host = $("#tbars"); host.innerHTML = "";
  $("#tsetsTitle").textContent = N.level ? `Level ${N.level + 1} items` : "Target sets";
  $("#tsetMeta").textContent = `${N.items.length} ${noun(N.items.length)}`;
  let cur = null, curBar = -2;
  N.items.forEach((it, i) => {
    const bar = t.bars[it.rows[0]];
    if (bar !== curBar || !cur) {
      curBar = bar;
      const b = el("div", "bar");
      b.append(el("span", "num", bar == null ? "" : `bar ${bar + 1}`));
      cur = el("div", "rows"); b.append(cur); host.append(b);
    }
    const chip = el("span", "row"); chip.dataset.trow = i;
    chip.append(el("i", null, i + 1), document.createTextNode(it.text.replace(/IC/g, "€")));
    if (N.level) chip.title = `${it.rows.length} ${it.rows.length === 1 ? "set" : "sets"} of the piece: ${ranges(it.rows)}`;
    chip.addEventListener("mouseenter", () => { N.thover = [i]; tlight(true); });
    chip.addEventListener("mouseleave", () => { N.thover = null; tlight(true); });
    chip.addEventListener("click", (e) => tpick(i, e.shiftKey));
    cur.append(chip);
  });
  tlight();
}

function tpick(item, range, items) {
  if (range && N.tanchor != null) {
    const [a, b] = [Math.min(N.tanchor, item), Math.max(N.tanchor, item)];
    for (let r = a; r <= b; r++) N.tsel.add(r);
  } else {
    const all = items && items.length ? items : [item];
    const on = all.every((r) => N.tsel.has(r));
    for (const r of all) on ? N.tsel.delete(r) : N.tsel.add(r);
    N.tanchor = item;
  }
  tlight();
}

const notesOf = (item) => (N.items[item] ? N.items[item].rows : [])
  .flatMap((r) => N.tnoteEls.get(r) || []);

/* Paint the target: picked items gold, hovered red, and each item's standing
   -- green where a checked cell reproduces it, red where one gets it wrong. */
function tlight(fromList) {
  const view = $("#targetView");
  view.querySelectorAll(".hot").forEach((n) => n.classList.remove("hot"));
  view.querySelectorAll(".picked").forEach((n) => n.classList.remove("picked"));
  view.querySelectorAll("#tbars .row").forEach((n) => {
    const i = +n.dataset.trow;
    n.classList.toggle("sel", N.tsel.has(i));
    n.classList.toggle("ok", N.cover.get(i) === "ok");
    n.classList.toggle("bad", N.cover.get(i) === "bad");
  });
  // The score shows the same standing as the list: written green, wrong red.
  N.items.forEach((_, i) => notesOf(i).forEach((g) => {
    g.classList.toggle("done", N.cover.get(i) === "ok");
    g.classList.toggle("wrong", N.cover.get(i) === "bad");
  }));
  for (const i of N.tsel) notesOf(i).forEach((g) => g.classList.add("picked"));
  let first = null;
  for (const i of N.thover || []) {
    notesOf(i).forEach((g) => g.classList.add("hot"));
    const ch = view.querySelector(`#tbars .row[data-trow="${i}"]`);
    if (ch) { ch.classList.add("hot"); first = first || ch; }
  }
  if (first && !fromList) first.scrollIntoView({ block: "nearest" });
  const n = N.tsel.size;
  $("#newFromSel").disabled = $("#tplaySel").disabled = $("#tclear").disabled = !n;
  $("#nextLevel").disabled = !N.complete;
  $("#nextLevel").title = N.complete
    ? `Make level ${N.level + 2} from the parts this level is written as`
    : "Unlocks when every item of this level is written, checks ✓, and is used once";
  const box = $("#treadout");
  if (N.thover && N.thover.length === 1 && N.items[N.thover[0]]) {
    const i = N.thover[0], it = N.items[i], t = N.target;
    const who = N.cells.find((c) => c.claim && c.claim.includes(i));
    box.innerHTML = `<b>${noun(1)} ${i + 1}</b> ${esc(it.text.replace(/IC/g, "€"))}`
      + (N.level ? ` · ${it.rows.length} ${it.rows.length === 1 ? "set" : "sets"}` : "")
      + (t.bars[it.rows[0]] != null ? ` · bar ${t.bars[it.rows[0]] + 1}` : "")
      + (who ? ` · in In[${who.n}]` : " · not written yet");
  } else if (n) {
    box.innerHTML = `<b>${n} selected</b> — ${noun(n)} ${ranges([...N.tsel])}`;
  } else {
    box.textContent = `Click ${noun(2)} or notes to select them, shift-click for a range, then make a cell from them.`;
  }
}

/* "3, 5–9, 12": indices, 1-based, runs collapsed. */
function ranges(idx) {
  const a = [...new Set(idx)].sort((x, y) => x - y), out = [];
  for (let k = 0; k < a.length; k++) {
    let j = k;
    while (j + 1 < a.length && a[j + 1] === a[j] + 1) j++;
    out.push(j > k ? `${a[k] + 1}–${a[j] + 1}` : `${a[k] + 1}`);
    k = j;
  }
  return out.join(", ");
}

/* The selected items as a new cell, a bar to a line, ready to be rewritten as
   concatenations. The cell remembers which items it stands for. */
function newCellFromSel() {
  const t = N.target, idx = [...N.tsel].sort((a, b) => a - b);
  if (!t || !idx.length) return;
  const lines = [];
  let bar = null;
  for (const i of idx) {
    const b = t.bars[N.items[i].rows[0]];
    if (!lines.length || b !== bar) { lines.push([]); bar = b; }
    lines[lines.length - 1].push(N.items[i].text);
  }
  const text = "{" + lines.map((l) => l.join(", ")).join(",\n ") + "}";
  // Replace a lone empty cell rather than leaving it above the new one.
  const blank = N.cells.length === 1 && !N.cells[0].ta.value.trim() ? N.cells[0] : null;
  const c = blank || addCell("", N.cells[N.cells.length - 1]);
  c.ta.value = text; size(c);
  c.claim = idx;
  N.tsel.clear(); N.tanchor = null;
  save();
  runCell(c, true).then(() => { coverage(); tlight(); });
  c.ta.focus();
  c.root.scrollIntoView({ block: "nearest", behavior: "smooth" });
  status(`In[${c.n}] stands for ${noun(idx.length)} ${ranges(idx)} — rewrite it with € and run it to check`);
}

/* Compare what a cell expands to with the piece's sets under the items it
   stands for. Whatever the level, the truth is the expansion. */
function check(c) {
  const t = N.target;
  const owner = [], wantIdx = [];
  for (const it of c.claim) for (const r of (N.items[it] || { rows: [] }).rows) {
    owner.push(it); wantIdx.push(r);
  }
  const want = wantIdx.map((r) => t.rows[r]), got = c.res.rows || [];
  const key = (r) => JSON.stringify(r);
  const bad = [];
  for (let k = 0; k < Math.max(want.length, got.length); k++)
    if (!want[k] || !got[k] || key(want[k]) !== key(got[k])) bad.push(k);
  const badItems = [...new Set(bad.map((k) => owner[Math.min(k, owner.length - 1)]))];
  if (!bad.length) return { ok: true, msg: `matches ${noun(2)} ${ranges(c.claim)}`, badItems };
  const k = bad[0];
  let msg;
  if (k >= got.length) msg = `stops after ${got.length} rows; the piece goes on to ${want.length}`;
  else if (k >= want.length) msg = `makes ${got.length} rows; the piece has ${want.length} here`;
  else {
    // Name the entries that differ, so the mistake is plain without comparing by eye.
    const names = columnsText(N.order).slice(1, -1).split(", ");
    const diff = want[k].map((v, j) => (key(v) !== key(got[k][j]) ? names[j] || `entry ${j + 1}` : null))
      .filter(Boolean);
    msg = `row ${k + 1} is {${got[k].join(",")}}, set ${wantIdx[k] + 1} of the piece is {${want[k].join(",")}}`
      + (diff.length && diff.length < want[k].length ? ` — the ${diff.join(" and ")} differs` : "");
  }
  if (bad.length > 1) msg += ` (${bad.length} rows differ)`;
  return { ok: false, msg, badItems };
}

async function checkCell(c) {
  if (!N.target) return;
  // With no items of its own yet, a cell stands for the selection, or failing
  // that for every item.
  if (!c.claim) c.claim = N.tsel.size ? [...N.tsel].sort((a, b) => a - b)
                                      : N.items.map((_, i) => i);
  save();
  await runCell(c);
  coverage();
  if (c.check) status(`In[${c.n}]: ${c.check.ok ? "✓ " : "✗ "}${c.check.msg}`, !c.check.ok);
}

function claimLine(c) {
  const line = el("div", "claim");
  line.append(el("span", null, `stands for ${noun(c.claim.length)} ${ranges(c.claim)}`));
  if (c.dirty) line.append(el("span", null, "· not checked since the edit"));
  else if (c.check) line.append(el("span", c.check.ok ? "ok" : "bad",
    (c.check.ok ? "✓ " : "✗ ") + (c.check.ok ? "matches" : c.check.msg)));
  const use = el("button", null, "use selection");
  use.title = "Make this cell stand for what is selected in the target";
  use.disabled = !N.tsel.size;
  use.onclick = () => { c.claim = [...N.tsel].sort((a, b) => a - b); save(); checkCell(c); };
  const drop = el("button", null, "✕");
  drop.title = "Stop checking this cell against the target";
  drop.onclick = () => { c.claim = null; c.check = null; save(); drawOut(c); coverage(); };
  line.append(use, drop);
  return line;
}

/* Which items the notebook reproduces so far, and whether the level is done:
   every item written, correct, and used by exactly one cell. */
function coverage() {
  N.cover = new Map();
  const uses = new Map();
  for (const c of N.cells) {
    if (!c.claim) continue;
    for (const i of c.claim) uses.set(i, (uses.get(i) || 0) + 1);
    if (!c.check || c.dirty) continue;
    const bad = new Set(c.check.badItems || []);
    for (const i of c.claim) {
      const st = bad.has(i) ? "bad" : "ok";
      if (N.cover.get(i) !== "bad") N.cover.set(i, st);
    }
  }
  const total = N.items.length;
  const ok = [...N.cover.values()].filter((v) => v === "ok").length;
  const wrong = [...N.cover.values()].filter((v) => v === "bad").length;
  N.twice = N.items.map((_, i) => i).filter((i) => (uses.get(i) || 0) > 1);
  N.complete = !!N.target && total > 0 && ok === total && !N.twice.length
    && N.cells.every((c) => !c.claim || (c.check && c.check.ok && !c.dirty));
  if (N.target) {
    $("#tcover").innerHTML = `<b>${ok}</b> of ${total} ${noun(total)} written`
      + (wrong ? ` · <span style="color:var(--accent)">${wrong} wrong</span>` : "")
      + (N.complete ? ` · <span style="color:#2f6b46">level ${N.level + 1} done ✓</span>` : "");
  }
  tlight(true);
}

async function checkAll() {
  if (!N.target) return;
  status("expanding every cell…");
  for (const c of [...N.cells]) if (c.ta.value.trim()) await runCell(c, true);
  coverage();
  const total = N.items.length;
  const ok = [...N.cover.values()].filter((v) => v === "ok").length;
  const bad = [...N.cover.values()].filter((v) => v === "bad").length;
  const missing = N.items.map((_, i) => i).filter((i) => !N.cover.has(i));
  let msg;
  if (N.complete) msg = `✓ level ${N.level + 1} is done: all ${total} ${noun(total)} written and correct`
    + ` — Next level is open`;
  else msg = `${ok} of ${total} ${noun(total)} written correctly`
    + (bad ? `, ${bad} wrong` : "") + (missing.length ? `; not yet written: ${ranges(missing)}` : "")
    + (N.twice.length ? `; used by more than one cell: ${ranges(N.twice)}` : "");
  $("#tinfo").textContent = msg;
  status(msg, !!bad);
}

/* ------------------------------------------------------------- resizing */
/* Two handles: the notebook against the music, and in each view the score
   against its sets. Sizes are remembered in this browser only. */
const SIZES = "musicic.notebook.sizes";
function splitters() {
  let sizes = {};
  try { sizes = JSON.parse(localStorage.getItem(SIZES) || "{}"); } catch {}
  const keep = () => { try { localStorage.setItem(SIZES, JSON.stringify(sizes)); } catch {} };
  const main = document.querySelector("main");
  const redraw = () => {
    if (N.tab === "target" && N.target) drawTarget(); else if (N.active && N.active.res) drawStage();
  };
  const setLeft = (px) => {
    const box = main.getBoundingClientRect();
    px = Math.max(300, Math.min(px, box.width - 420));
    main.style.setProperty("--left", px + "px");
    sizes.left = px;
  };
  const setPaper = (view, px) => {
    const paper = view.querySelector(".paper");
    px = Math.max(110, Math.min(px, window.innerHeight - 260));
    paper.style.flex = `0 0 ${px}px`; paper.style.maxHeight = "none";
    sizes[view.id] = px;
  };
  if (sizes.left) setLeft(sizes.left);
  for (const v of [$("#targetView"), $("#outView")]) if (sizes[v.id]) setPaper(v, sizes[v.id]);

  // One drag routine for both kinds of handle.
  const drag = (handle, onMove, done) => {
    handle.addEventListener("pointerdown", (e) => {
      e.preventDefault(); handle.setPointerCapture(e.pointerId);
      document.body.classList.add("dragging");
      const move = (ev) => onMove(ev);
      const up = () => {
        handle.removeEventListener("pointermove", move);
        document.body.classList.remove("dragging"); keep(); done && done();
      };
      handle.addEventListener("pointermove", move);
      handle.addEventListener("pointerup", up, { once: true });
      handle.addEventListener("pointercancel", up, { once: true });
    });
  };
  const vs = $("#vsplit");
  const leftNow = () => document.querySelector(".book").getBoundingClientRect().width;
  drag(vs, (ev) => setLeft(ev.clientX - main.getBoundingClientRect().left - 18), redraw);
  vs.addEventListener("keydown", (e) => {
    const d = e.key === "ArrowLeft" ? -30 : e.key === "ArrowRight" ? 30 : 0;
    if (d) { e.preventDefault(); setLeft(leftNow() + d); keep(); redraw(); }
  });
  vs.addEventListener("dblclick", () => { main.style.removeProperty("--left"); delete sizes.left; keep(); redraw(); });

  document.querySelectorAll(".hsplit").forEach((h) => {
    const view = h.closest(".view"), paper = view.querySelector(".paper");
    drag(h, (ev) => setPaper(view, ev.clientY - paper.getBoundingClientRect().top));
    h.addEventListener("keydown", (e) => {
      const d = e.key === "ArrowUp" ? -30 : e.key === "ArrowDown" ? 30 : 0;
      if (d) { e.preventDefault(); setPaper(view, paper.getBoundingClientRect().height + d); keep(); }
    });
    h.addEventListener("dblclick", () => {
      paper.style.flex = ""; paper.style.maxHeight = ""; delete sizes[view.id]; keep();
    });
  });
}

/* ---------------------------------------------------------- save / open */
/* A notebook saves to the project's notebooks/ folder as Markdown: readable on
   its own, with the exact state inside for reopening. It keeps the file it was
   saved to, so later saves overwrite it. */
async function saveBook() {
  save();
  const b = B.books[B.current];
  status("saving…");
  try {
    const j = await api("/api/notebooks/save", { book: b, file: b.file || null });
    b.file = j.file; save(); loadSaved();
    status(`saved to notebooks/${j.file}`);
  } catch (err) { status("could not save: " + err.message, true); }
}

async function loadSaved() {
  const sel = $("#savedIn");
  try {
    const j = await fetch("/api/notebooks").then((r) => r.json());
    sel.innerHTML = "";
    sel.append(el("option", null, j.files.length ? `saved (${j.files.length})…` : "nothing saved yet"));
    sel.options[0].value = "";
    for (const f of j.files) {
      const when = new Date(f.saved * 1000).toLocaleString([], { month: "short", day: "numeric",
        hour: "2-digit", minute: "2-digit" });
      const o = el("option", null, f.error ? `${f.file} — will not open`
        : `${f.name}${f.levels > 1 ? ` · ${f.levels} levels` : ""} · ${when}`);
      o.value = f.file; o.disabled = !!f.error; o.title = f.error || f.file;
      sel.append(o);
    }
  } catch { sel.innerHTML = "<option value=''>saved notebooks unavailable</option>"; }
}

async function openSaved(file) {
  // The notebook this file was saved from, if it is here; otherwise a new one.
  const id = Object.keys(B.books).find((k) => B.books[k].file === file) || "file:" + file;
  if (id === B.current &&
      !confirm("Reopen the saved copy of this notebook? Changes since it was saved will be lost.")) return;
  status("opening notebooks/" + file + "…");
  try {
    const j = await api("/api/notebooks/open", { file });
    B.books[id] = { ...j.book, file };
    await openBook(id);
    status(`opened notebooks/${file}, as it was left`);
  } catch (err) { status("could not open: " + err.message, true); }
}

/* ---------------------------------------------------------------- export */
const EXPORT_PREFS = "musicic.notebook.export";

function exportChoices(read) {
  const form = $("#exportDlg form");
  const names = ["math", "score", "items", "code"];
  if (read) {
    const o = Object.fromEntries(names.map((n) => [n, form.elements[n].checked]));
    try { localStorage.setItem(EXPORT_PREFS, JSON.stringify(o)); } catch {}
    return o;
  }
  let o = null;
  try { o = JSON.parse(localStorage.getItem(EXPORT_PREFS) || "null"); } catch {}
  if (o) for (const n of names) form.elements[n].checked = !!o[n];
}

function openExport() {
  exportChoices(false);
  // Sheet music is drawn from the piece, so it needs a notebook with one.
  const score = $("#exportDlg form").elements.score;
  score.disabled = !N.target;
  score.parentElement.title = N.target ? "" : "Sheet music comes from the piece: start the notebook from a MIDI file or a library piece";
  $("#exportDlg").showModal();
}

async function exportPdf(opts) {
  save();
  const b = B.books[B.current];
  status("typesetting…");
  try {
    const images = opts.score && N.target ? await levelImages(b) : {};
    const r = await fetch("/api/export/pdf", { method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ book: b, options: opts, images }) });
    if (!r.ok || !(r.headers.get("content-type") || "").includes("pdf")) {
      const j = await r.json().catch(() => ({}));
      throw new Error(j.error || `${r.status} ${r.statusText}`);
    }
    const blob = await r.blob();
    const a = el("a");
    a.href = URL.createObjectURL(blob);
    a.download = (b.name || "notebook").replace(/[^\w\s-]/g, "").trim().replace(/\s+/g, "-") + ".pdf";
    document.body.append(a); a.click(); a.remove();
    setTimeout(() => URL.revokeObjectURL(a.href), 30000);
    status(`exported ${a.download}`);
  } catch (err) { status("could not export: " + err.message, true); }
}

/* The piece's score as PNG: once plain, and once per level with every note
   coloured by the block that makes it, as the notebook colours them. */
async function levelImages(b) {
  if (!N.tview) await loadTarget(N.gen);
  if (!N.tview) return {};
  const t = b.target, s = settings();
  const out = { piece: await scorePng(N.tview.score, new Map()) };
  for (let k = 0; k < b.levels.length; k++) {
    const lv = b.levels[k];
    const items = lv.items || t.rows.map((r, i) => ({ rows: [i] }));
    const colour = new Map();                 // target row -> css colour
    for (let n = 0; n < (lv.cells || []).length; n++) {
      const text = lv.cells[n], claim = (lv.claims || [])[n];
      if (!text || !text.trim() || !claim) continue;
      let j;
      try { j = await api("/api/ic", { text, ...s }); } catch { continue; }
      const rows = claim.flatMap((i) => (items[i] || { rows: [] }).rows);
      const blocks = (j.ic.sources && j.ic.sources.blocks) || [];
      for (const sp of [...(j.ic.spans || [])].sort((x, y) => x.depth - y.depth)) {
        const idx = blocks.findIndex((bk) => bk.id === sp.id);
        for (let r = sp.start; r <= sp.end; r++)
          if (rows[r] != null) colour.set(rows[r], hue(Math.max(idx, 0)));
      }
    }
    out[`L${k + 1}`] = await scorePng(N.tview.score, colour);
  }
  return out;
}

async function scorePng(score, colour) {
  const host = el("div");
  host.style.cssText = "position:fixed;left:-12000px;top:0;width:1000px;background:#fff";
  document.body.append(host);
  try {
    const { noteEls } = engrave(host, score, { barNumbers: true, lineH: 118 });
    for (const [row, c] of colour)
      for (const g of noteEls.get(row) || [])
        g.querySelectorAll("path").forEach((p) => { p.style.fill = c; p.style.stroke = c; });
    const svg = host.querySelector("svg");
    if (!svg) return null;
    const w = +svg.getAttribute("width"), h = +svg.getAttribute("height");
    const xml = new XMLSerializer().serializeToString(svg);
    const img = new Image();
    img.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(xml);
    await img.decode();
    const k = 2.5, canvas = el("canvas");
    canvas.width = Math.round(w * k); canvas.height = Math.round(h * k);
    const ctx = canvas.getContext("2d");
    ctx.fillStyle = "#fff"; ctx.fillRect(0, 0, canvas.width, canvas.height);
    ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
    return canvas.toDataURL("image/png");
  } finally { host.remove(); }
}

/* --------------------------------------------------------- column order */
/* The order of a set's entries -- {duration, pitch} or {pitch, duration}, and
   for a triple any order of start, duration and pitch. It is how the rows are
   written and shown; the server reads them back in the encoder's own order. */
const COLUMN_NAMES = { dpitch: "Δpitch", ioi: "gap" };
const baseColumns = () => ((N.encs || {})[$("#encoder").value] || { columns: [] }).columns;
const columnsText = (order) => {
  const cols = baseColumns();
  return "{" + (order || cols.map((_, i) => i)).map((i) => COLUMN_NAMES[cols[i]] || cols[i]).join(", ") + "}";
};

function permutations(n) {
  if (n <= 1) return [[0]].slice(0, n ? 1 : 0).concat(n ? [] : [[]]);
  const out = [];
  for (const p of permutations(n - 1))
    for (let k = 0; k <= p.length; k++) out.push([...p.slice(0, k), n - 1, ...p.slice(k)]);
  return out.sort((a, b) => a.join() < b.join() ? -1 : 1);
}

function columnMenu() {
  const sel = $("#columns"), cols = baseColumns();
  sel.innerHTML = "";
  for (const p of permutations(cols.length)) {
    const o = el("option", null, columnsText(p)); o.value = p.join(","); sel.append(o);
  }
  sel.value = (N.order || cols.map((_, i) => i)).join(",");
  sel.disabled = cols.length < 2;
}

/* Put every set in a new order: the target, each level's items, and every
   cell's expression, rewritten entry by entry so it still expands to match. */
async function changeOrder(next) {
  const cols = baseColumns(), ident = cols.map((_, i) => i);
  const now = N.order || ident;
  const rel = next.map((c) => now.indexOf(c));          // new entry k = old entry rel[k]
  if (rel.every((v, k) => v === k)) return;
  save();
  const b = B.books[B.current];
  status(`rewriting the sets as ${columnsText(next)}…`);
  let failed = 0;
  const rewrite = async (text) => {
    if (!text || !text.trim()) return text;
    try { return (await api("/api/ic/reorder", { text, order: rel })).text; }
    catch { failed++; return text; }
  };
  for (const lv of b.levels) {
    lv.cells = await Promise.all((lv.cells || []).map(rewrite));
    if (lv.items) for (const it of lv.items) it.text = await rewrite(it.text);
  }
  if (b.target) b.target.rows = b.target.rows.map((r) => rel.map((j) => r[j]));
  b.order = next.every((v, k) => v === k) ? null : next;
  try { localStorage.setItem(STORE, JSON.stringify(B)); } catch {}
  await openBook(B.current);
  status(failed ? `sets are now ${columnsText(next)}; ${failed} cell(s) could not be read and were left as they were`
                : `sets are now ${columnsText(next)} — every cell rewritten to match`, !!failed);
}

/* ------------------------------------------------------------- play all */
/* Play the whole song. A notebook started from a piece plays the piece, and
   follows along: each note lights up on the target score as it sounds, with
   its set and the cell that stands for it. Otherwise every cell of this level
   plays top to bottom, each starting where the one before ended. */
let playTimers = [];

function stopPlaying() {
  stopAll();
  playTimers.forEach(clearTimeout); playTimers = [];
  document.querySelectorAll(".sounding").forEach((n) => n.classList.remove("sounding"));
  N.cells.forEach((c) => c.root.classList.remove("playing"));
}

const later = (ms, fn) => playTimers.push(setTimeout(fn, Math.max(0, ms)));
const clock = (secs) => `${Math.floor(secs / 60)}:${String(Math.round(secs) % 60).padStart(2, "0")}`;

async function playAll() {
  audio();                       // unlock sound now, while the click still counts
  stopPlaying();
  if (N.target) return playSong();
  // A cell edited since it last ran is run first, so what plays is what is written.
  for (const c of [...N.cells])
    if (c.ta.value.trim() && (!c.res || c.dirty)) await runCell(c, true);
  const cells = N.cells.filter((c) => c.res && c.res.view.playback.notes.length);
  if (!cells.length) { status("nothing to play yet — write a cell and run it", true); return; }
  const notes = [];
  let at = 0;
  for (const c of cells) {
    const pb = c.res.view.playback;
    const start = at;
    later(start * 1000, () => {
      N.cells.forEach((x) => x.root.classList.toggle("playing", x === c));
      c.root.scrollIntoView({ block: "nearest", behavior: "smooth" });
    });
    for (const n of pb.notes) notes.push({ t: at + n.t, d: n.d, p: n.p });
    at += pb.end || Math.max(...pb.notes.map((n) => n.t + n.d));
  }
  playNotes({ notes, end: at }, null);
  later(at * 1000 + 200, () => N.cells.forEach((x) => x.root.classList.remove("playing")));
  status(`playing all ${cells.length} cell${cells.length > 1 ? "s" : ""}, ${notes.length} notes, ${clock(at)}`);
}

async function playSong() {
  if (!N.tview) { status("drawing the piece…"); await loadTarget(N.gen); }
  const pb = N.tview && N.tview.playback;
  if (!pb || !pb.notes.length) { status("this piece has no notes to play", true); return; }
  if (N.tab !== "target") showTab("target");
  playNotes(pb, null);
  // playNotes starts the first note at once; the lights keep the same clock.
  const base = pb.notes[0].t;
  let last = null;
  for (const n of pb.notes) {
    const on = (n.t - base) * 1000, off = on + Math.max(n.d * 1000 * 0.92, 60);
    later(on, () => {
      const els = N.tnoteEls.get(n.r) || [];
      els.forEach((g) => g.classList.add("sounding"));
      const item = N.itemOf ? N.itemOf.get(n.r) : null;
      const chip = item != null && $(`#tbars .row[data-trow="${item}"]`);
      if (chip) { chip.classList.add("sounding"); chip.scrollIntoView({ block: "nearest" }); }
      if (els[0] && els[0] !== last) {
        last = els[0];
        els[0].scrollIntoView({ block: "nearest", behavior: "smooth" });
      }
      const cell = item != null && N.cells.find((c) => c.claim && c.claim.includes(item));
      N.cells.forEach((c) => c.root.classList.toggle("playing", c === cell));
    });
    later(off, () => {
      (N.tnoteEls.get(n.r) || []).forEach((g) => g.classList.remove("sounding"));
      const item = N.itemOf ? N.itemOf.get(n.r) : null;
      if (item != null) $(`#tbars .row[data-trow="${item}"]`)?.classList.remove("sounding");
    });
  }
  const total = (pb.end || pb.notes[pb.notes.length - 1].t + pb.notes[pb.notes.length - 1].d) - base;
  later(total * 1000 + 200, () => N.cells.forEach((c) => c.root.classList.remove("playing")));
  status(`playing the whole song — ${pb.notes.length} notes, ${clock(total)}`);
}
