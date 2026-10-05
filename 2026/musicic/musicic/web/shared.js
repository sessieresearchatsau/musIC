/* musIC — what the workbench and the sheet page share: fetch helpers, the
   engraver and the synth. Loaded before either page's own script. */
const $ = (s) => document.querySelector(s);
const el = (t, cls, txt) => { const n = document.createElement(t);
  if (cls) n.className = cls; if (txt != null) n.textContent = txt; return n; };
const esc = (s) => String(s).replace(/[&<>]/g,
  (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;" }[c]));

async function api(path, body, isForm) {
  const opts = isForm ? { method: "POST", body }
    : { method: "POST", headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body) };
  const r = await fetch(path, opts);
  const j = await r.json().catch(() => ({ error: `${r.status} ${r.statusText}` }));
  if (j && j.error) throw new Error(j.error);
  return j;
}

/* ------------------------------------------------------------- engraver */
/* Draw `score` (the server's layout) into `host` as SVG.

   opts.lineH       vertical room per system
   opts.barNumbers  number the first bar of every system after the first
   opts.onNote(g, sn)  called for each drawn note that came from set rows;
                    sn._rows holds those row indices

   Returns { noteEls: Map(row -> [svg group]), staves: [geometry] }. */
function engrave(host, score, opts = {}) {
  host.innerHTML = "";
  const noteEls = new Map(), staves = [];
  if (!score || !score.measures.length) {
    host.append(el("div", "body sub", "nothing to draw"));
    return { noteEls, staves };
  }
  const VF = Vex.Flow;
  const width = Math.max(host.clientWidth - 4, 300);   // fits a phone
  const usable = width - 24;
  const FIRST_EXTRA = 74;          // clef + key signature + time signature
  const lineH = opts.lineH || 118;
  const top = opts.barNumbers ? 20 : 12;   // headroom for the bar numbers

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
  r.resize(width, lines.length * lineH + top + 14);
  const ctx = r.getContext(); ctx.setFont("sans-serif", 9);

  // A tied note waits here until the note it ties into has been formatted, which
  // may be in the next bar or on the next system.
  let pendingTie = null;
  const tie = (first, last) => {
    const idx = (sn) => sn && sn.getKeys().map((_, k) => k);
    try {
      new VF.StaveTie({ first_note: first, last_note: last,
        first_indices: idx(first || last), last_indices: idx(last || first) })
        .setContext(ctx).draw();
    } catch (e) { /* a tie is decoration; never lose the bar over one */ }
  };

  lines.forEach((idxs, row) => {
    // Justify: scale this line's bars so they exactly span the page.
    const extra = FIRST_EXTRA;
    const raw = idxs.reduce((s, i) => s + want[i], 0);
    let scale = Math.max(0.55, (usable - extra) / Math.max(raw, 1));
    // A last line well short of the margin keeps its natural width, as an
    // engraver leaves it, rather than stretching one bar across the page.
    if (row === lines.length - 1 && lines.length > 1 && raw + extra < usable * 0.6)
      scale = Math.min(scale, 1.15);
    let x = 12;
    idxs.forEach((i, col) => {
      const m = score.measures[i];
      const w = want[i] * scale + (col === 0 ? extra : 0);
      const stave = new VF.Stave(x, top + row * lineH, w);
      if (col === 0) {
        stave.addClef("treble");
        if (score.key && score.key !== "C") stave.addKeySignature(score.key);
        if (row === 0) stave.addTimeSignature(`${score.numerator}/${score.denominator}`);
      }
      stave.setContext(ctx).draw();
      staves.push({ x, w, top: stave.getYForLine(0),
                    spacing: stave.getYForLine(1) - stave.getYForLine(0) });
      // Engraving convention: a system's first bar carries its number, except
      // the very first bar of the piece.
      if (opts.barNumbers && col === 0 && m.index > 0) {
        ctx.save(); ctx.setFont("serif", 11, "italic");
        ctx.fillText(String(m.index + 1), x + 2, stave.getYForLine(0) - 12);
        ctx.restore();
      }

      const notes = m.notes.map((n) => {
        const sn = new VF.StaveNote({
          keys: n.keys, duration: n.code + (n.rest ? "r" : ""),
          clef: "treble", autoStem: true, dots: n.dots,
        });
        // `dots` above gives the note its true length; this only draws the dot.
        // With the glyph alone VexFlow timed a dotted quarter as a quarter,
        // which misplaced every beam group and spacing after it in the bar.
        for (let d = 0; d < n.dots; d++) VF.Dot.buildAndAttach([sn], { all: true });
        // Only the accidentals the layout said to draw: the key signature and
        // the rest of the bar cover the others.
        (n.accidentals || []).forEach((a, k) => {
          if (a) sn.addModifier(new VF.Accidental(a), k);
        });
        sn._rows = n.rows || []; sn._label = n.label;
        sn._code = n.code; sn._dots = n.dots; sn._tie = n.tie && !n.rest;
        return sn;
      });

      let beams = [];
      try {
        const voice = new VF.Voice({ num_beats: score.numerator,
          beat_value: score.denominator }).setStrict(false);
        voice.addTickables(notes);
        // Beam by beat, the way an engraver would: a run of sixteenths in 4/4
        // breaks into groups of four rather than one bar-long beam.
        // Pass the rests too: grouping walks the bar by ticks, so dropping them
        // shifted every later note into the wrong beat. Compound metres beam in
        // dotted-quarter groups, three eighths to a beat.
        const compound = score.denominator === 8 && score.numerator % 3 === 0
          && score.numerator > 3;
        beams = VF.Beam.generateBeams(notes, {
          groups: [compound ? new VF.Fraction(3, 8)
                            : new VF.Fraction(1, score.denominator)],
        });
        new VF.Formatter().joinVoices([voice]).format([voice],
          Math.max(w - (col === 0 ? extra + 16 : 18), 60));
        voice.draw(ctx, stave);
        beams.forEach((b) => b.setContext(ctx).draw());
      } catch (e) { /* one bad bar must not blank the page */ }

      notes.forEach((sn) => {
        if (pendingTie && !sn.isRest()) {
          // A tie that wraps to a new system is drawn as two halves: one
          // leaving the end of the old line, one arriving at the new.
          if (pendingTie.row === row) tie(pendingTie.sn, sn);
          else { tie(pendingTie.sn, null); tie(null, sn); }
        }
        pendingTie = sn._tie ? { sn, row } : null;
      });

      notes.forEach((sn) => {
        const g = sn.getSVGElement && sn.getSVGElement();
        if (!g || !sn._rows.length) return;
        g.classList.add("vf-note");
        g.dataset.rows = sn._rows.join(",");
        for (const rw of sn._rows) {
          if (!noteEls.has(rw)) noteEls.set(rw, []);
          noteEls.get(rw).push(g);
        }
        if (opts.onNote) opts.onNote(g, sn);
      });
      x += w;
    });
  });

  if (pendingTie) tie(pendingTie.sn, null);   // continues onto the next page
  return { noteEls, staves };
}

/* --------------------------------------------------------------- synth */
let AC = null, playing = [];
/* Browsers start sound only in answer to a click: an AudioContext made or
   left suspended outside one stays silent. Call this first thing in a click
   handler, before anything is awaited. */
function audio() {
  AC = AC || new (window.AudioContext || window.webkitAudioContext)();
  if (AC.state === "suspended") AC.resume();
  return AC;
}
function stopAll() { playing.forEach((o) => { try { o.stop(); } catch {} }); playing = []; }

/* Play the server's playback list, or only the rows in `sel` when it has any.
   Returns how many notes were scheduled. */
function playNotes(pb, sel) {
  if (!pb || !pb.notes.length) return 0;
  stopAll();
  const ac = audio(), t0 = ac.currentTime + 0.06;
  const picked = sel && sel.size ? [...sel] : [];
  const notes = picked.length
    ? pb.notes.filter((n, i) => picked.includes(n.r ?? i))
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
  return notes.length;
}
