/* musIC — the concatenate page. Paste any set of integer rows; the server's
   reducer (core/concat.py) returns nested indexed concatenations that expand
   back to exactly those rows. The reference sets from
   docs/concatenation-examples.md load from the menu, with what Mathematica's
   ReduceSetList made of each, for comparison. */

const C = {
  examples: [],      // from /api/concatenate/examples
  example: null,     // the reference set loaded, until the text is edited
  res: null,         // last result
  view: "euro",      // which form the result panel shows
  busy: false,
};

function status(msg, bad) {
  const n = $("#status"); n.textContent = n.title = msg || "";
  n.style.color = bad ? "var(--accent)" : "var(--dim)";
}

const fmt = (n) => Number(n).toLocaleString();

/* -------------------------------------------------------------- startup */
(async function init() {
  $("#run").onclick = run;
  $("#clear").onclick = () => { $("#input").value = ""; C.example = null;
    $("#examples").value = ""; countRows(); $("#input").focus(); };
  $("#examples").onchange = () => loadExample($("#examples").value);
  $("#input").addEventListener("input", () => {
    C.example = null; $("#examples").value = ""; countRows();
  });
  $("#input").addEventListener("keydown", (e) => {
    if ((e.metaKey || e.ctrlKey) && e.key === "Enter") { e.preventDefault(); run(); }
  });
  $("#tabEuro").onclick = () => showForm("euro");
  $("#tabWl").onclick = () => showForm("wl");
  $("#copy").onclick = copyResult;

  try {
    const j = await fetch("/api/concatenate/examples").then((r) => r.json());
    C.examples = j.examples || [];
    for (const e of C.examples) {
      const o = el("option", null, `${e.title} — ${fmt(e.rows)} rows`);
      o.value = e.id; $("#examples").append(o);
    }
  } catch { status("reference sets unavailable", true); }

  // /concatenate?example=3 opens that reference set and runs it.
  const q = new URLSearchParams(location.search);
  if (q.get("example")) { loadExample(q.get("example")); if (C.example) run(); }
})();

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
  $("#examples").value = id;
  countRows();
  status(e.limit
    ? `${e.title}: the first ${fmt(e.rows)} rows, as Mathematica was given them`
    : `${e.title} loaded`);
}

/* ------------------------------------------------------------------ run */
async function run() {
  const text = $("#input").value.trim();
  if (!text) { status("paste a set first", true); $("#input").focus(); return; }
  if (C.busy) return;
  C.busy = true;
  const btn = $("#run");
  btn.disabled = true;
  btn.innerHTML = '<span class="busy" aria-hidden="true"></span>Concatenating…';
  status("searching for blocks — large sets take several seconds");
  try {
    const res = await api("/api/concatenate", { text });
    C.res = res;
    draw(res);
    status(`${fmt(res.rows)} rows → ${fmt(res.leaves)} leaves in ${res.seconds} s`);
  } catch (err) {
    status(err.message, true);
  } finally {
    C.busy = false;
    btn.disabled = false;
    btn.textContent = "Concatenate";
  }
}

/* ----------------------------------------------------------------- draw */
function stat(value, label, win) {
  const d = el("div", "stat" + (win ? " win" : ""));
  d.append(el("div", "v", value), el("div", "l", label));
  return d;
}

function draw(res) {
  $("#empty").hidden = true;
  $("#summary").hidden = $("#result").hidden = $("#parts").hidden = false;

  const s = $("#stats"); s.innerHTML = "";
  s.append(stat(fmt(res.rows), "rows in the set"));
  s.append(stat(fmt(res.runs_only), "leaves, repeats only"));
  const mm = C.example && C.example.mathematica_leaves;
  s.append(stat(fmt(res.leaves), "leaves, concatenated",
    mm != null ? res.leaves < mm : res.leaves < res.runs_only));
  if (mm != null) s.append(stat(fmt(mm), "leaves, Mathematica"));
  s.append(stat(res.seconds + " s", "time"));

  const v = $("#verdict");
  v.className = "verdict " + (res.verified ? "ok" : "bad");
  let msg = res.verified
    ? `✓ Expands back to all ${fmt(res.rows)} rows exactly.`
    : "✗ No reduction could be verified, so only repeated rows are collapsed.";
  if (res.verified && mm != null) {
    msg += res.leaves < mm ? ` ${fmt(mm - res.leaves)} fewer leaves than ReduceSetList.`
      : res.leaves === mm ? " Same size as ReduceSetList."
      : ` ${fmt(res.leaves - mm)} more leaves than ReduceSetList.`;
  }
  v.textContent = msg;

  showForm(C.view);

  const body = $("#partRows"); body.innerHTML = "";
  for (const p of res.parts) {
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
  const blocks = res.parts.filter((p) => p.kind === "block");
  const covered = blocks.reduce((a, p) => a + p.rows, 0);
  $("#partsNote").textContent = blocks.length
    ? `${blocks.length} indexed block${blocks.length > 1 ? "s" : ""} cover${blocks.length > 1 ? "" : "s"} ${fmt(covered)} of ${fmt(res.rows)} rows`
    : "no indexed block found";
}

function showForm(which) {
  C.view = which;
  $("#tabEuro").setAttribute("aria-selected", which === "euro");
  $("#tabWl").setAttribute("aria-selected", which === "wl");
  if (C.res) $("#expr").textContent = which === "euro" ? C.res.text : C.res.mathematica;
}

async function copyResult() {
  if (!C.res) return;
  const t = C.view === "euro" ? C.res.text : C.res.mathematica;
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
