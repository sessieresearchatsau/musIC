/* musIC — the sheet page. The piece engraved as a printed page, with its set
   beside it, row by row and bar by bar. The workbench is still where sets are
   built and measured; this page is for reading a piece against its set. */

const SHEET_BARS = 48;           // bars per sheet; a longer piece turns pages

const T = {
  session: null, track: 0, file: null,   // file: the library entry, if any
  name: "", source: "",
  view: null, page: 0,
  sel: new Set(), noteEls: new Map(),
  rowBar: new Map(),                     // row -> bar index, for this page
};

function status(msg, bad) {
  const n = $("#status"); n.textContent = n.title = msg || "";
  n.style.color = bad ? "var(--accent)" : "var(--dim)";
}

/* -------------------------------------------------------------- startup */
(async function init() {
  const encs = await (await fetch("/api/encoders")).json();
  for (const e of encs) {
    const o = el("option", null, e.title + (e.invertible ? "" : "  (lossy)"));
    o.value = e.key; $("#encoder").append(o);
  }
  $("#encoder").value = "pair";

  $("#file").onchange = (e) => e.target.files[0] && openMidi(e.target.files[0]);
  $("#setfile").onchange = (e) => e.target.files[0] && openSetFile(e.target.files[0]);
  $("#library").onchange = () => $("#library").value && openLibrary($("#library").value);
  $("#track").onchange = (e) => { T.track = +e.target.value; T.page = 0; refresh(); };
  $("#encoder").onchange = () => refresh();
  $("#reduction").onchange = () => { T.page = 0; refresh(); };
  $("#prevPg").onclick = () => turn(-1);
  $("#nextPg").onclick = () => turn(1);
  $("#play").onclick = () => play(false);
  $("#playSel").onclick = () => play(true);
  $("#stop").onclick = stopAll;
  $("#clearSel").onclick = () => { T.sel.clear(); syncSel(); };
  $("#copy").onclick = copySet;
  window.addEventListener("resize", () => T.view && drawSheet());

  const q = new URLSearchParams(location.search);
  if (q.get("encoder")) $("#encoder").value = q.get("encoder");
  if (q.get("reduction")) $("#reduction").value = q.get("reduction");
  await loadLibrary(q.get("file"));
  if (q.get("file")) await openLibrary(q.get("file"));
  else if (q.get("session")) await resume(q.get("session"), +q.get("track") || 0);
})();

async function loadLibrary(keep) {
  const sel = $("#library");
  try {
    const j = await fetch("/api/library").then((r) => r.json());
    sel.innerHTML = "";
    const head = el("option", null,
      j.files.length ? `${j.files.length} in scores/…` : "scores/ is empty");
    head.value = ""; sel.append(head);
    for (const f of j.files) {
      const o = el("option", null, f.error ? `${f.file} — will not parse` : f.name);
      o.value = f.file; o.disabled = !!f.error; o.title = f.error || f.source || f.file;
      sel.append(o);
    }
    if (keep) sel.value = keep;
  } catch { sel.innerHTML = "<option value=''>library unavailable</option>"; }
}

/* ---------------------------------------------------------------- open */
function setTracks(tracks) {
  const t = $("#track"); t.innerHTML = "";
  tracks.forEach((tr, i) => {
    const o = el("option", null, tracks.length > 1
      ? `${tr.name} — ${tr.notes} notes` : tr.name);
    o.value = i; t.append(o);
  });
  t.value = T.track;
}

async function openLibrary(file) {
  status("opening " + file + "…");
  try {
    const j = await api("/api/library/open", { file });
    T.session = j.session; T.track = 0; T.file = file; T.page = 0; T.sel.clear();
    T.name = j.setfile.name; T.source = j.setfile.source || "";
    if (j.setfile.encoder) $("#encoder").value = j.setfile.encoder;
    $("#library").value = file;
    setTracks([j.track]);
    await refresh();
  } catch (err) { status(err.message, true); }
}

async function openSetFile(f) {
  status("reading " + f.name + "…");
  const fd = new FormData(); fd.append("file", f);
  try {
    const j = await api("/api/setfile", fd, true);
    T.session = j.session; T.track = 0; T.file = null; T.page = 0; T.sel.clear();
    T.name = j.setfile.name; T.source = j.setfile.source || f.name;
    if (j.setfile.encoder) $("#encoder").value = j.setfile.encoder;
    $("#library").value = "";
    setTracks([j.track]);
    await refresh();
  } catch (err) { status(err.message, true); }
}

async function openMidi(f) {
  status("reading " + f.name + "…");
  const fd = new FormData(); fd.append("file", f); fd.append("grid", "32");
  try {
    const j = await api("/api/midi", fd, true);
    T.session = j.session; T.track = 0; T.file = null; T.page = 0; T.sel.clear();
    T.name = ""; T.source = j.filename;
    $("#library").value = "";
    setTracks(j.tracks);
    await refresh();
  } catch (err) { status(err.message, true); }
}

/* Pick up a piece the workbench already loaded. Sessions live in the server's
   memory, so a link from before a restart has nothing to resume. */
async function resume(session, track) {
  try {
    const j = await fetch("/api/session?session=" + encodeURIComponent(session))
      .then((r) => r.json());
    if (j.error) throw new Error(j.error);
    T.session = session; T.track = Math.min(track, j.tracks.length - 1);
    T.name = j.tracks.length === 1 ? j.tracks[0].name : "";
    T.source = j.tracks[0].source || "";
    setTracks(j.tracks);
    await refresh();
  } catch (err) { status(err.message, true); }
}

/* -------------------------------------------------------------- refresh */
async function refresh() {
  if (!T.session) return;
  try {
    T.view = await api("/api/view", {
      session: T.session, track: T.track, page: T.page,
      encoder: $("#encoder").value, reduction: $("#reduction").value,
      max_measures: SHEET_BARS,
    });
    T.page = T.view.score.page || 0;
    drawSheet(); drawSet(); remember();
    status(`${T.view.encoding.rows.length} rows · ${T.view.score.total_measures} bars`);
  } catch (err) { status(err.message, true); }
}

function turn(d) {
  T.page = Math.max(0, T.page + d); refresh();
  $(".paper").scrollIntoView({ block: "start", behavior: "smooth" });
}

/* Keep the address bar pointing at what is open, so the sheet can be
   bookmarked, and so the Workbench tab opens the same piece. */
function remember() {
  const q = new URLSearchParams();
  if (T.file) q.set("file", T.file);
  else { q.set("session", T.session); if (T.track) q.set("track", T.track); }
  q.set("encoder", $("#encoder").value);
  if ($("#reduction").value !== "top") q.set("reduction", $("#reduction").value);
  history.replaceState(null, "", "/sheet?" + q);
  $("#toBench").href = "/?" + q;
  $("#toNotebook").href = "/notebook?" + q;
}

/* ---------------------------------------------------------------- sheet */
function drawSheet() {
  const v = T.view, sc = v.score, tr = v.track;
  const title = T.name || tr.name || "Untitled";
  $("#title").textContent = title;
  document.title = `${title} — musIC sheet`;
  $("#subtitle").textContent = T.source && T.source !== title ? T.source : "";
  $("#marks").hidden = false;
  $("#tempo").textContent = `♩ = ${Math.round(sc.tempo || 120)}`;
  $("#keyMark").textContent = [sc.key_display, `${sc.numerator}/${sc.denominator}`]
    .filter(Boolean).join(" · ");

  const out = engrave($("#score"), sc, {
    barNumbers: true, lineH: 132,
    onNote: (g, sn) => {
      g.addEventListener("mouseenter", () => hover(sn._rows[0], true));
      g.addEventListener("mouseleave", () => hover(null));
      g.addEventListener("click", () => sn._rows.forEach(toggleSel));
    },
  });
  T.noteEls = out.noteEls;

  const pages = sc.pages || 1;
  $("#folio").hidden = pages < 2;
  $("#pageNo").textContent = `page ${T.page + 1} of ${pages}`;
  $("#prevPg").disabled = T.page <= 0;
  $("#nextPg").disabled = T.page >= pages - 1;
  syncSel();
}

/* ------------------------------------------------------------------ set */
/* The rows, grouped under the bar their note sits in -- the set read the way
   the page reads. A note tied across a barline belongs to the bar it starts in. */
function drawSet() {
  const v = T.view, enc = v.encoding, host = $("#bars");
  host.innerHTML = "";
  $("#setTitle").textContent = enc.title;
  T.rowBar = new Map();
  for (const m of v.score.measures)
    for (const n of m.notes)
      for (const r of n.rows || []) if (!T.rowBar.has(r)) T.rowBar.set(r, m.index);

  if (enc.nested) {
    // A grouped encoding has no row-per-note, so there is nothing to link.
    $("#setMeta").textContent = `${enc.rows.length} groups`;
    enc.rows.forEach((g, gi) => {
      const b = el("div", "bar");
      b.append(el("span", "num", (enc.labels && enc.labels[gi]) || `#${gi}`));
      const rows = el("div", "rows");
      g.forEach((r) => rows.append(el("span", "row", "{" + r.join(",") + "}")));
      if (!g.length) rows.append(el("span", "row", "{}"));
      b.append(rows); host.append(b);
    });
    return;
  }

  const onPage = enc.rows.map((_, i) => i).filter((i) => T.rowBar.has(i));
  $("#setMeta").textContent = onPage.length === enc.rows.length
    ? `${enc.rows.length} rows`
    : `rows ${onPage[0] + 1}–${onPage[onPage.length - 1] + 1} of ${enc.rows.length}`;

  let cur = null, curBar = -1;
  for (const i of onPage) {
    const bar = T.rowBar.get(i);
    if (bar !== curBar) {
      curBar = bar;
      const b = el("div", "bar"); b.dataset.bar = bar;
      b.append(el("span", "num", `bar ${bar + 1}`));
      cur = el("div", "rows"); b.append(cur); host.append(b);
    }
    const chip = el("span", "row");
    chip.dataset.row = i;
    chip.append(el("i", null, i + 1));
    chip.append(document.createTextNode("{" + enc.rows[i].join(",") + "}"));
    chip.addEventListener("mouseenter", () => hover(i, false));
    chip.addEventListener("mouseleave", () => hover(null));
    chip.addEventListener("click", () => toggleSel(i));
    cur.append(chip);
  }
}

/* -------------------------------------------------------------- linking */
function hover(row, fromNote) {
  document.querySelectorAll(".hot").forEach((n) => n.classList.remove("hot"));
  if (row == null) { $("#readout").textContent = "Hover a note or a row to link them."; return; }
  (T.noteEls.get(row) || []).forEach((g) => g.classList.add("hot"));
  const chip = document.querySelector(`#bars .row[data-row="${row}"]`);
  if (chip) {
    chip.classList.add("hot");
    chip.closest(".bar").classList.add("hot");
    // Follow the pointer only when it is on the staff; scrolling the list
    // under a pointer that is on the list would move the row away from it.
    if (fromNote) chip.scrollIntoView({ block: "nearest" });
  }
  const enc = T.view.encoding, vals = enc.rows[row];
  const desc = vals ? enc.columns.map((c, k) => `${c} ${vals[k]}`).join(" · ") : "";
  const note = noteInfo(row);
  $("#readout").innerHTML = `<b>row ${row + 1}</b> &nbsp;${esc(desc)}`
    + (note ? `<br>→ <b>${esc(note)}</b>` : "");
}

function noteInfo(row) {
  for (const m of T.view.score.measures) for (const n of m.notes)
    if ((n.rows || []).includes(row))
      return `${n.label} · ${n.code}${".".repeat(n.dots)} · bar ${m.index + 1}`;
  return "";
}

function toggleSel(row) {
  if (T.sel.has(row)) T.sel.delete(row); else T.sel.add(row);
  syncSel();
}

function syncSel() {
  document.querySelectorAll("#bars .row[data-row]").forEach((n) =>
    n.classList.toggle("sel", T.sel.has(+n.dataset.row)));
  document.querySelectorAll(".picked").forEach((n) => n.classList.remove("picked"));
  for (const row of T.sel)
    (T.noteEls.get(row) || []).forEach((g) => g.classList.add("picked"));
  $("#playSel").disabled = $("#clearSel").disabled = !T.sel.size;
  $("#selInfo").textContent = T.sel.size
    ? `${T.sel.size} selected — rows ${[...T.sel].sort((a, b) => a - b)
        .map((i) => i + 1).join(", ")}`.slice(0, 140)
    : "Click rows or noteheads to mark a subsequence.";
}

/* ----------------------------------------------------------- play, copy */
function play(selectionOnly) {
  const n = playNotes(T.view && T.view.playback, selectionOnly ? T.sel : null);
  if (n) status(`playing ${n} notes${selectionOnly ? " (selection)" : ""}`);
}

async function copySet() {
  if (!T.view) return;
  const enc = T.view.encoding;
  const text = T.sel.size && !enc.nested
    ? "{" + [...T.sel].sort((a, b) => a - b)
        .map((i) => "{" + enc.rows[i].join(",") + "}").join(",") + "}"
    : enc.mathematica;
  try { await navigator.clipboard.writeText(text);
    status(T.sel.size ? "copied the selection" : "copied the set"); }
  catch { status("copy blocked by the browser", true); }
}
