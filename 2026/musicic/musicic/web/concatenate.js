/* musIC — the concatenate page. Paste any set of integer rows and pick an
   approach (core/strategies.py); the server returns nested indexed
   concatenations that expand back to exactly those rows. "Compare all" runs
   every approach on the set. The reference sets from
   docs/concatenation-examples.md load from the menu, with what Mathematica's
   ReduceSetList made of each, and so do the pieces in scores/, with their bar
   lines. */

const C = {
  examples: [],      // from /api/concatenate/examples
  example: null,     // the reference set loaded, until the text is edited
  bars: null,        // the bar of each row, from the piece loaded
  strategies: [],    // from /api/concatenate/strategies
  res: null,         // the result on show
  compared: [],      // results of Compare all
  view: "euro",      // which notation the result panel shows
  busy: false,
};

function status(msg, bad) {
  const n = $("#status"); n.textContent = n.title = msg || "";
  n.style.color = bad ? "var(--accent)" : "var(--dim)";
}

const fmt = (n) => Number(n).toLocaleString();

/* -------------------------------------------------------------- startup */
(async function init() {
  $("#run").onclick = () => run(false);
  $("#compareAll").onclick = () => run(true);
  $("#clear").onclick = () => { $("#input").value = ""; C.example = null; C.bars = null;
    $("#examples").value = ""; countRows(); noteBars(); $("#input").focus(); };
  $("#examples").onchange = () => loadExample($("#examples").value);
  $("#input").addEventListener("input", () => {
    // the bar lines stay with the rows; the server drops them if the count changes
    C.example = null; $("#examples").value = ""; countRows();
    $("#compare").hidden = true;           // it compared the set as it was
  });
  $("#input").addEventListener("keydown", (e) => {
    if ((e.metaKey || e.ctrlKey) && e.key === "Enter") { e.preventDefault(); run(); }
  });
  $("#tabEuro").onclick = () => showForm("euro");
  $("#tabWl").onclick = () => showForm("wl");
  $("#copy").onclick = copyResult;
  $("#exportBtn").onclick = openExport;
  $("#exportDlg").addEventListener("close", () => {
    if ($("#exportDlg").returnValue === "ok") exportPdf(exportChoices(true));
  });

  try {
    const j = await fetch("/api/concatenate/strategies").then((r) => r.json());
    C.strategies = j.strategies || [];
    drawApproaches(j);
  } catch { status("approaches unavailable", true); }
  try {
    const j = await fetch("/api/concatenate/examples").then((r) => r.json());
    C.examples = j.examples || [];
    const groups = { ref: el("optgroup"), piece: el("optgroup") };
    groups.ref.label = "Reference sets"; groups.piece.label = "Pieces, with bar lines";
    for (const e of C.examples) {
      const o = el("option", null, `${e.title} — ${fmt(e.rows)} rows`);
      o.value = e.id; (e.piece ? groups.piece : groups.ref).append(o);
    }
    for (const g of Object.values(groups)) if (g.children.length) $("#examples").append(g);
  } catch { status("reference sets unavailable", true); }
  fillStrategies();

  // /concatenate?example=3 opens that reference set and runs it.
  const q = new URLSearchParams(location.search);
  if (q.get("strategy")) { $("#strategy").value = q.get("strategy"); }
  if (q.get("example")) { loadExample(q.get("example")); if (C.example) run(false); }
})();

/* The approach menu. One that goes bar by bar is offered only when the set
   came with bar lines. */
function fillStrategies() {
  const sel = $("#strategy"), keep = sel.value || "S·U";
  sel.innerHTML = "";
  for (const s of C.strategies) {
    const o = el("option", null, `${s.code} — ${s.name}`
      + (s.needs_bars && !C.bars ? " (needs bar lines)" : ""));
    o.value = s.code; o.title = s.blurb;
    o.disabled = s.needs_bars && !C.bars;
    sel.append(o);
  }
  sel.value = [...sel.options].some((o) => o.value === keep && !o.disabled) ? keep : "S·U";
}

function noteBars() {
  $("#barsNote").textContent = C.bars
    ? `bar lines: ${fmt(new Set(C.bars).size)} bars, from the piece` : "";
  fillStrategies();
}

/* Count rows as the person types, so a paste that lost its end shows at once.
   This is only a guide: the server's parser is the one that decides. */
function countRows() {
  const t = $("#input").value;
  const inner = t.match(/\{[^{}]*\}/g);
  $("#count").textContent = inner && t.trim() ? `≈ ${fmt(inner.length)} rows` : "";
}

function loadExample(id) {
  const e = C.examples.find((x) => String(x.id) === String(id));
  if (!e) return;
  $("#input").value = e.text;
  C.example = e;
  C.bars = e.bars || null;
  $("#examples").value = id;
  countRows();
  noteBars();
  status(e.limit
    ? `${e.title}: the first ${fmt(e.rows)} rows, as Mathematica was given them`
    : `${e.title} loaded`);
}

/* ------------------------------------------------------------------ run */
/* The search streams what it tries (/api/concatenate/stream), so the
   Attempts panel fills while it runs; the result arrives as its last line. */
async function run(all) {
  const text = $("#input").value.trim();
  if (!text) { status("paste a set first", true); $("#input").focus(); return; }
  if (C.busy) return;
  C.busy = true;
  const btn = all ? $("#compareAll") : $("#run");
  const label = btn.textContent;
  $("#run").disabled = $("#compareAll").disabled = true;
  btn.innerHTML = `<span class="busy" aria-hidden="true"></span>${all ? "Comparing…" : "Concatenating…"}`;
  status(all ? "running every approach, one after another"
    : "searching for blocks — watch the attempts as they come in");
  traceReset();
  if (all) compareReset();
  const strategy = $("#strategy").value;
  try {
    const r = await fetch(all ? "/api/concatenate/compare" : "/api/concatenate/stream", {
      method: "POST", headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ text, strategy, bars: C.bars }) });
    if (!(r.headers.get("content-type") || "").includes("ndjson")) {
      const j = await r.json().catch(() => ({}));
      throw new Error(j.error || `${r.status} ${r.statusText}`);
    }
    const reader = r.body.getReader(), dec = new TextDecoder();
    let buf = "", end = null;
    for (;;) {
      const { value, done } = await reader.read();
      if (done) break;
      buf += dec.decode(value, { stream: true });
      let nl;
      while ((nl = buf.indexOf("\n")) >= 0) {
        const line = buf.slice(0, nl); buf = buf.slice(nl + 1);
        if (!line.trim()) continue;
        const ev = JSON.parse(line);
        if (ev.t === "done" || ev.t === "stopped" || ev.t === "error") end = ev;
        else if (ev.t === "result") compareAdd(ev);
        else traceEvent(ev);
      }
    }
    traceFlush();
    traceFinish(end);
    if (!end) throw new Error("the search ended without a result");
    if (end.t === "error") throw new Error(end.error);
    if (end.t === "stopped") { status(`stopped after ${end.seconds} s`); return; }
    if (all) {
      const best = compareBest();
      if (best) { draw(best); markPicked(best.code); }
      status(`${fmt(end.rows)} rows: ${C.compared.filter((x) => !x.skipped).length} approaches compared`);
    } else {
      draw(end);
      status(`${end.code}: ${fmt(end.rows)} rows → ${fmt(end.leaves)} rows written, `
        + `${fmt(end.measures.numbers)} numbers, in ${end.seconds} s`);
    }
  } catch (err) {
    status(err.message, true);
  } finally {
    C.busy = false;
    T.id = null;
    $("#stop").hidden = true;
    $("#run").disabled = $("#compareAll").disabled = false;
    btn.textContent = label;
  }
}

/* ------------------------------------------------------------- compare */
function compareReset() {
  C.compared = [];
  $("#cmpRows").innerHTML = "";
  $("#cmpNote").textContent = "";
  $("#compare").hidden = false;
}

const key = (x) => [x.verified ? 0 : 1, x.leaves, x.measures.numbers];
const before = (a, b) => { const ka = key(a), kb = key(b);
  for (let i = 0; i < ka.length; i++) if (ka[i] !== kb[i]) return ka[i] < kb[i];
  return false; };

function compareBest() {
  let best = null;
  for (const x of C.compared) if (!x.skipped && (!best || before(x, best))) best = x;
  return best;
}

function compareAdd(res) {
  C.compared.push(res);
  const tr = el("tr");
  tr.dataset.code = res.code;
  tr.append(el("td", "code", `${res.code}`));
  tr.firstChild.title = res.name;
  if (res.skipped) {
    const td = el("td", "n", `— ${res.skipped}`); td.colSpan = 7; tr.append(td);
  } else {
    const m = res.measures;
    tr.append(el("td", "n", fmt(m.rows) + (res.verified ? "" : " ✗")), el("td", "n", fmt(m.numbers)),
      el("td", "n", fmt(m.parts)), el("td", "n", fmt(m.depth)),
      el("td", "n", m.tools.filter((x) => x !== "E1").join(" ") || "E1"),
      el("td", "n", m.bar_fit == null ? "—" : `${Math.round(m.bar_fit * 100)}%`),
      el("td", "n", `${res.seconds} s`));
    tr.onclick = () => { draw(res); markPicked(res.code); };
  }
  $("#cmpRows").append(tr);
  const best = compareBest();
  document.querySelectorAll("#cmpRows tr").forEach((r) =>
    r.classList.toggle("best", !!best && r.dataset.code === best.code));
  $("#cmpNote").textContent = best
    ? `fewest rows, then numbers: ${best.code} · click a row to see it` : "";
}

function markPicked(code) {
  document.querySelectorAll("#cmpRows tr").forEach((r) =>
    r.classList.toggle("sel", r.dataset.code === code));
}

/* ----------------------------------------------------------- approaches */
function drawApproaches(j) {
  const body = $("#apRows"); body.innerHTML = "";
  for (const s of j.strategies || []) {
    const tr = el("tr");
    tr.append(el("td", "code", s.code), el("td", null, s.name), el("td", null, s.blurb));
    body.append(tr);
  }
  const corpus = j.corpus, rows = $("#corpusRows"); rows.innerHTML = "";
  if (!corpus) { $("#corpusNote").textContent = "· not run yet"; return; }
  const sets = Object.values(corpus.sets);
  $("#corpusNote").textContent = `· ${sets.length} sets, ${corpus.limit} s limit each`;
  const median = (xs) => { if (!xs.length) return null;
    const s = [...xs].sort((a, b) => a - b), m = s.length >> 1;
    return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2; };
  const wins = Object.fromEntries(corpus.strategies.map((c) => [c, 0]));
  for (const set of sets) {
    const done = Object.entries(set.runs).filter(([, d]) => d.lossless);
    if (!done.length) continue;
    const top = Math.min(...done.map(([, d]) => d.rows));
    const atTop = done.filter(([, d]) => d.rows === top);
    const nums = Math.min(...atTop.map(([, d]) => d.numbers));
    for (const [c, d] of atTop) if (d.numbers === nums) wins[c]++;
  }
  for (const c of corpus.strategies) {
    const ran = sets.map((s) => [s, s.runs[c] || {}]).filter(([, d]) => !d.skipped);
    const fin = ran.filter(([, d]) => d.rows != null);
    const ok = fin.filter(([, d]) => d.lossless);
    const r = median(ok.map(([s, d]) => d.rows / s.n));
    const nm = median(ok.map(([s, d]) => d.numbers / s.n));
    const tm = median(fin.map(([, d]) => d.seconds));
    const tr = el("tr");
    tr.append(el("td", "code", c), el("td", "n", `${fin.length} of ${ran.length}`),
      el("td", "n", `${ok.length}`), el("td", "n", `${wins[c]}`),
      el("td", "n", r == null ? "—" : r.toFixed(3)), el("td", "n", nm == null ? "—" : nm.toFixed(2)),
      el("td", "n", tm == null ? "—" : `${tm} s`));
    rows.append(tr);
  }
}

/* ------------------------------------------------------------- attempts */
const T = {
  id: null,          // the running search, for Stop
  n: 0,              // rows in the set
  rows: [],          // the rows, for showing an attempt's
  pending: [],       // events not drawn yet (drawn once per frame)
  tries: 0,
  rounds: 0,
  listed: 0,
  kept: [],          // [r0, r1) of every block kept
  round: null,       // [r0, r1) being searched now
  look: null,        // the attempt on the map: the latest, or the one picked
  picked: null,
  frame: 0,
};
const MAX_LISTED = 3000;

function traceReset() {
  Object.assign(T, { id: null, n: 0, rows: [], pending: [], tries: 0, rounds: 0, listed: 0,
    kept: [], round: null, look: null, picked: null });
  $("#tries").innerHTML = "";
  $("#pick").textContent = "Click an attempt to see its rows.";
  $("#traceNote").textContent = "starting…";
  $("#empty").hidden = true;
  $("#summary").hidden = $("#result").hidden = $("#parts").hidden = true;
  $("#trace").hidden = false;
  drawMap();
}

function traceEvent(ev) {
  if (ev.t === "start") {
    T.id = ev.id; T.n = ev.rows; T.rows = ev.rowsText || [];
    $("#mapEnd").textContent = fmt(ev.rows);
    $("#stop").hidden = false;
    return;
  }
  T.pending.push(ev);
  if (!T.frame) T.frame = requestAnimationFrame(traceFlush);
}

const span = (a, b) => (b - a === 1 ? `row ${a + 1}` : `rows ${a + 1}–${b}`);

function whyOf(ev) {
  switch (ev.status) {
    case "best": return ev.saving == null ? "grouped" : `new best: saves ${fmt(ev.saving)} leaves`;
    case "beaten": if (ev.saving == null) return "fits, but another grouping is cheaper";
      return ev.saving === ev.best
      ? `saves ${fmt(ev.saving)}, ties the best but its formulas are no simpler`
      : `saves ${fmt(ev.saving)}, beaten by the best so far (${fmt(ev.best)})`;
    case "nofit": return "no formula fits every column of its periods";
    case "short": return "checked row by row, it repeats fewer than 2 times";
    case "refit": return "could not be re-indexed to run from k = 1";
    case "loose": return "the same shape twice, but too many of its numbers change";
  }
  return "";
}

function traceFlush() {
  T.frame = 0;
  const list = $("#tries");
  const atEnd = list.scrollHeight - list.scrollTop - list.clientHeight < 40;
  for (const ev of T.pending.splice(0)) {
    let li = null;
    if (ev.t === "round") {
      T.rounds++; T.round = [ev.r0, ev.r1];
      li = el("li", "round", ev.label || `Round ${ev.n} · searching ${span(ev.r0, ev.r1)}`);
    } else if (ev.t === "try") {
      T.tries++;
      T.look = ev;
      li = el("li");
      const label = { best: "best", beaten: "beaten", nofit: "no fit", short: "too short",
        refit: "no refit" }[ev.status] || ev.status;
      li.append(el("span", "st " + ev.status, label),
        el("span", "w", span(ev.r0, ev.r1)));
      const why = el("span", "why", ev.cut
        ? `cutting ${ev.cut}: ${fmt(ev.pieces)} pieces, grouped up to ${fmt(ev.parts)} part${ev.parts === 1 ? "" : "s"} over ${ev.levels} level${ev.levels === 1 ? "" : "s"}`
          + (ev.status === "best" ? " — the best so far" : " — no better")
        : `period ${ev.p}${ev.q ? ` × ${ev.q}` : ""}${ev.pass === 2 ? " · single rows" : ""} — ${whyOf(ev)}`);
      if (ev.text) { const x = el("span", "x", ev.text); x.title = ev.text; why.append(x); }
      li.append(why);
    } else if (ev.t === "keep" || ev.t === "none") {
      if (ev.t === "keep") T.kept.push([ev.r0, ev.r1]);
      T.look = null;
      li = el("li");
      li.append(el("span", "st " + ev.t, ev.t === "keep" ? "kept" : "none"),
        el("span", "w", span(ev.r0, ev.r1)));
      const why = el("span", "why", ev.t === "keep"
        ? (ev.saving == null ? "kept"
          : `this block saves ${fmt(ev.saving)} leaves; the rows either side are searched next`)
        : ev.why || "no block saves anything here, so only repeated rows are collapsed");
      if (ev.text) { const x = el("span", "x", ev.text); x.title = ev.text; why.append(x); }
      li.append(why);
    }
    if (!li) continue;
    if (T.listed >= MAX_LISTED) continue;
    T.listed++;
    if (ev.t !== "round") {
      li.onclick = () => pickTry(ev, li);
      li.onmouseenter = () => { T.look = ev; drawMap(); };
      li.onmouseleave = () => { T.look = T.picked; drawMap(); };
    }
    list.append(li);
  }
  if (atEnd) list.scrollTop = list.scrollHeight;
  noteCounts(C.busy ? "searching…" : "");
  drawMap();
}

function noteCounts(extra) {
  $("#traceNote").textContent = `${fmt(T.tries)} tried · ${fmt(T.kept.length)} kept · `
    + `${fmt(T.rounds)} round${T.rounds === 1 ? "" : "s"}`
    + (T.listed >= MAX_LISTED ? ` · first ${fmt(MAX_LISTED)} listed` : "")
    + (extra ? ` · ${extra}` : "");
}

function traceFinish(end) {
  T.round = null; T.look = T.picked;
  noteCounts(end && end.t === "stopped" ? "stopped" : end && end.t === "done" ? "finished" : "");
  drawMap();
}

function pickTry(ev, li) {
  document.querySelectorAll("#tries li.sel").forEach((x) => x.classList.remove("sel"));
  if (li) li.classList.add("sel");
  T.picked = T.look = ev;
  drawMap();
  const p = $("#pick"); p.innerHTML = "";
  p.append(el("b", null, span(ev.r0, ev.r1)), document.createTextNode(` · ${fmt(ev.r1 - ev.r0)} rows`));
  const rows = T.rows.slice(ev.r0, Math.min(ev.r1, ev.r0 + 8));
  if (rows.length) {
    p.append(document.createTextNode(": "),
      el("span", "mono", rows.join(", ") + (ev.r1 - ev.r0 > rows.length ? ", …" : "")));
  }
}

/* The rows as a strip, one cell per row: kept blocks, the stretch being
   searched, and the attempt looked at. */
function drawMap() {
  const cv = $("#map");
  const dpr = window.devicePixelRatio || 1;
  const w = cv.clientWidth || 300, h = 30;
  if (cv.width !== Math.round(w * dpr)) { cv.width = Math.round(w * dpr); cv.height = h * dpr; }
  const g = cv.getContext("2d");
  g.setTransform(dpr, 0, 0, dpr, 0, 0);
  g.clearRect(0, 0, w, h);
  if (!T.n) return;
  const x = (i) => (i / T.n) * w;
  const bar = (a, b, y, hh, c) => {
    g.fillStyle = c; g.fillRect(x(a), y, Math.max(1.5, x(b) - x(a)), hh);
  };
  if (T.round) bar(T.round[0], T.round[1], 0, h, "#d9d4cb");
  for (const [a, b] of T.kept) bar(a, b, 0, h, "#7fb38c");
  if (T.look) bar(T.look.r0, T.look.r1, 4, h - 8, "rgba(245,215,110,.9)");
  if (T.n <= 120) {                   // few rows: show where each one is
    g.fillStyle = "rgba(255,255,255,.7)";
    for (let i = 1; i < T.n; i++) g.fillRect(x(i), 0, 1, h);
  }
}

$("#stop").onclick = async () => {
  if (!T.id) return;
  $("#stop").disabled = true;
  try { await api("/api/concatenate/stop", { id: T.id }); status("stopping…"); }
  catch (err) { status(err.message, true); }
  finally { $("#stop").disabled = false; }
};
window.addEventListener("resize", drawMap);

/* ----------------------------------------------------------------- draw */
function stat(value, label, win) {
  const d = el("div", "stat" + (win ? " win" : ""));
  d.append(el("div", "v", value), el("div", "l", label));
  return d;
}

/* The answer on show: the approach just run, or the one picked in the
   comparison. */
const answer = (res) => res;

function draw(res) {
  C.res = res;
  $("#empty").hidden = true;
  $("#summary").hidden = $("#result").hidden = $("#parts").hidden = false;
  $("#levels").hidden = !(res.levels && res.levels.length);
  $("#resWho").textContent = `${res.code} · ${res.name}`;
  $("#resWho").title = res.blurb || "";

  const m = res.measures, s = $("#stats"); s.innerHTML = "";
  s.append(stat(fmt(res.rows), "rows in the set"));
  s.append(stat(fmt(res.runs_only), "rows written, repeats only"));
  s.append(stat(fmt(res.leaves), `rows written, ${res.code}`, res.leaves < res.runs_only));
  s.append(stat(fmt(m.numbers), "numbers written"));
  s.append(stat(`${fmt(m.parts)} · ${fmt(m.depth)}`, "parts · depth"));
  s.append(stat(m.tools.join(" "), "€ tools used"));
  if (m.bar_fit != null) s.append(stat(`${Math.round(m.bar_fit * 100)}%`, "bar fit"));
  const mm = C.example && C.example.mathematica_leaves;
  if (mm != null) s.append(stat(fmt(mm), "rows, Mathematica"));
  s.append(stat(res.seconds + " s", "time"));

  drawLevels(res);
  showAnswer();
}

function showAnswer() {
  if (!C.res) return;
  const a = answer(C.res);
  const mm = C.example && C.example.mathematica_leaves;

  const v = $("#verdict");
  // Expanding back exactly is only worth a ✓ when something was concatenated:
  // the rows written out as they are expand back too.
  const found = a.parts.some((p) => p.kind === "block");
  const runs = a.parts.some((p) => p.kind === "repeat");
  v.className = "verdict " + (!a.verified ? "bad" : found ? "ok" : "none");
  let msg = !a.verified
    ? "✗ No reduction could be verified, so only repeated rows are collapsed."
    : found ? `✓ Concatenated, and expands back to all ${fmt(C.res.rows)} rows exactly.`
    : runs ? "No concatenation found: only rows that repeat in a row were collapsed."
    : "No concatenation found: the result is just the rows written out. "
      + "See Attempts for what was tried.";
  if (a.verified && found && mm != null) {
    msg += a.leaves < mm ? ` ${fmt(mm - a.leaves)} fewer rows than ReduceSetList.`
      : a.leaves === mm ? " Same size as ReduceSetList."
      : ` ${fmt(a.leaves - mm)} more rows than ReduceSetList.`;
  }
  v.textContent = msg;

  showForm(C.view);

  const body = $("#partRows"); body.innerHTML = "";
  for (const p of a.parts) {
    const tr = el("tr");
    const last = p.first + p.rows;
    tr.append(el("td", "n", p.rows === 1 ? `${p.first + 1}` : `${p.first + 1}–${last}`));
    tr.append(el("td", "n", fmt(p.rows)));
    const k = el("td");
    k.append(el("span", "kind " + p.kind,
      { block: "indexed block", repeat: "repeat", row: "row" }[p.kind]));
    tr.append(k);
    tr.append(el("td", "x", p.text));
    body.append(tr);
  }
  const blocks = a.parts.filter((p) => p.kind === "block");
  const covered = blocks.reduce((n, p) => n + p.rows, 0);
  $("#partsNote").textContent = blocks.length
    ? `${blocks.length} indexed block${blocks.length > 1 ? "s" : ""} cover${blocks.length > 1 ? "" : "s"} ${fmt(covered)} of ${fmt(C.res.rows)} rows`
    : "no indexed block found";
}

/* Each level of the level-by-level answer, its parts in order. Clicking one
   lights up its rows on the row map. */
function drawLevels(res) {
  const host = $("#levelList"); host.innerHTML = "";
  const lv = res.levels || [];
  $("#levelsNote").textContent = lv.length
    ? `${fmt(res.rows)} rows → ` + lv.map((l) => fmt(l.length)).join(" → ") : "";
  lv.forEach((parts, k) => {
    const box = el("div", "lv");
    const from = k === 0 ? `${fmt(res.rows)} rows` : `${fmt(lv[k - 1].length)} ${k === 1 ? "pieces" : "parts"}`;
    const h = el("h3", null, `Level ${k + 1} `);
    h.append(el("span", "sub", `· ${from} → ${fmt(parts.length)} ${k === 0 ? "pieces" : "parts"}`));
    const ol = el("ol");
    for (const p of parts) {
      const li = el("li", p.kind);
      li.append(el("span", "w", span(p.r0, p.r1)), el("span", "x", p.text));
      li.onclick = () => {
        document.querySelectorAll(".lv li.sel").forEach((x) => x.classList.remove("sel"));
        li.classList.add("sel");
        pickTry({ r0: p.r0, r1: p.r1 }, null);
      };
      ol.append(li);
    }
    box.append(h, ol);
    host.append(box);
  });
}

function showForm(which) {
  C.view = which;
  $("#tabEuro").setAttribute("aria-selected", which === "euro");
  $("#tabWl").setAttribute("aria-selected", which === "wl");
  if (C.res) {
    const a = answer(C.res);
    $("#expr").textContent = which === "euro" ? a.text : a.mathematica;
  }
}

async function copyResult() {
  if (!C.res) return;
  const a = answer(C.res);
  const t = C.view === "euro" ? a.text : a.mathematica;
  try {
    await navigator.clipboard.writeText(t);
    status(C.view === "euro" ? "copied in € notation"
      : "copied as IndexedConcatenate — ExpandAll gives the set back with SSSiCv102.wl loaded");
  } catch {
    // clipboard refused (no permission): select it so ⌘C works
    const r = document.createRange(); r.selectNodeContents($("#expr"));
    const sel = getSelection(); sel.removeAllRanges(); sel.addRange(r);
    status("selected — press ⌘C to copy");
  }
}

/* ---------------------------------------------------------------- export */
/* As the Notebook does it: a dialog of what goes in, remembered between
   visits, then the server typesets it with xelatex and the PDF downloads. */
const EXPORT_PREFS = "musicic.concatenate.export";
const EXPORT_PARTS = ["set", "approach", "math", "code", "measures", "structure", "levels", "compare"];

function exportChoices(read) {
  const form = $("#exportDlg form");
  if (read) {
    const o = Object.fromEntries(EXPORT_PARTS.map((n) => [n, form.elements[n].checked]));
    try { localStorage.setItem(EXPORT_PREFS, JSON.stringify(o)); } catch {}
    o.title = form.elements.title.value.trim();
    return o;
  }
  let o = null;
  try { o = JSON.parse(localStorage.getItem(EXPORT_PREFS) || "null"); } catch {}
  if (o) for (const n of EXPORT_PARTS) if (n in o) form.elements[n].checked = !!o[n];
}

const compared = () => C.compared.filter((x) => !x.skipped);

function openExport() {
  if (!C.res) { status("concatenate the set first, then export", true); return; }
  exportChoices(false);
  const f = $("#exportDlg form").elements;
  if (!f.title.value) f.title.value = (C.example && C.example.title) || "Concatenation";
  // Only what this result has can go in.
  const has = { levels: !!(C.res.levels && C.res.levels.length), compare: compared().length > 1 };
  for (const [n, ok] of Object.entries(has)) {
    f[n].disabled = !ok;
    f[n].parentElement.title = ok ? "" : n === "compare"
      ? "Press Compare all first" : "This approach does not build levels";
  }
  $("#exportDlg").showModal();
}

async function exportPdf(opts) {
  const text = $("#input").value.trim();
  const f = $("#exportDlg form").elements;
  const options = Object.fromEntries(EXPORT_PARTS.map((n) => [n, opts[n] && !f[n].disabled]));
  const name = opts.title || "Concatenation";
  // a result travels without what the PDF does not use
  const slim = (r) => ({ code: r.code, name: r.name, text: r.text, measures: r.measures,
    seconds: r.seconds, levels: r.levels, skipped: r.skipped });
  status("typesetting…");
  try {
    const r = await fetch("/api/export/concatenate", { method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ name, text, options, result: slim(C.res),
        compared: options.compare ? C.compared.map(slim) : null }) });
    if (!r.ok || !(r.headers.get("content-type") || "").includes("pdf")) {
      const j = await r.json().catch(() => ({}));
      throw new Error(j.error || `${r.status} ${r.statusText}`);
    }
    const blob = await r.blob();
    const a = el("a");
    a.href = URL.createObjectURL(blob);
    a.download = name.replace(/[^\w\s-]/g, "").trim().replace(/\s+/g, "-") + ".pdf";
    document.body.append(a); a.click(); a.remove();
    setTimeout(() => URL.revokeObjectURL(a.href), 30000);
    status(`exported ${a.download}`);
  } catch (err) { status("could not export: " + err.message, true); }
}
