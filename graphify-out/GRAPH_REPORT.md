# Graph Report - musIC  (2026-10-14)

## Corpus Check
- 157 files · ~174,432 words
- Verdict: corpus is large enough that graph structure adds value.
- Unclassified: 79 file(s) not represented in the graph (top: .nb 22, .mid 21, (none) 12)

## Summary
- 3295 nodes · 8022 edges · 148 communities (62 shown, 86 thin omitted)
- Extraction: 96% EXTRACTED · 3% INFERRED · 0% AMBIGUOUS · INFERRED: 279 edges (avg confidence: 0.9)
- Token cost: 271,496 input · 0 output

## Community Hubs (Navigation)
- RuleFlow Vec Cells
- Notebook Page UI
- concat.py Reducer Design
- Server API Endpoints
- RuleFlow Core Helpers
- RuleFlow Delta Engine
- Levels of Concatenation
- VexFlow Element Base
- Workbench Page UI
- IC Expression Rendering
- RuleFlow Music Plugins
- icn.py EDSL Tool
- VexFlow Metrics
- reduce.py Port
- Concat vs Reduce Comparison
- Note Encoders
- VexFlow Stave System
- FlowLang Parser
- VexFlow Canvas Text
- RuleFlow Space States
- VexFlow Path Primitives
- VexFlow Modifier Context
- VexFlow Bounding Geometry
- VexFlow Accidentals
- FlowLang Interpreter
- Notebook File Format
- RuleFlow Number Library
- Set File Format
- VexFlow Glyph Styles
- VexFlow Stave Modifiers
- Key Signatures
- VexFlow Stave Connectors
- VexFlow Voice Ticks
- FlowLang Explore Plugin
- ICN Paper Concepts
- VexFlow Canvas Paths
- VexFlow Module Root
- VexFlow Formatter
- VexFlow Stems Flags
- RuleFlow Vec Caching
- VexFlow Beams
- Sequence Finding (findseq)
- VexFlow Tick Contexts
- concat.py Block Search
- Set File Checking
- VexFlow Fonts Elements
- VexFlow Beam Drawing
- New Reducer Docs
- VexFlow Key Signatures
- VexFlow Music Theory
- VexFlow Text Glyphs
- MIDI Import Quantize
- Concatenate Page UI
- VexFlow Text Lines
- Transcription Rules
- RuleFlow Events
- VexFlow Accidental Apply
- VexFlow Renderer Contexts
- VexFlow Rest Alignment
- VexFlow Tables
- concat.py Atoms Bodies
- VexFlow Annotations
- VexFlow Chord Builder
- Experiment 1 Variants
- Sheet Page UI
- VexFlow Ties Curves
- VexFlow Font Sizes
- VexFlow Stem Extensions
- RuleFlow Signals
- VexFlow Font Sections
- VexFlow Modifier Format
- VexFlow Padding
- VexFlow Parser
- VexFlow Tablature
- RuleFlow Studio Signals
- MIDI Export
- Server Concat Routes
- VexFlow Clef Props
- VexFlow Justification
- RuleFlow Docs
- RuleFlow Studio Model
- Melody Metrics
- VexFlow Tick Formatting
- RuleFlow Editor Actions
- midi2txt5 Script
- VexFlow Barlines
- concat.py Formulas
- VexFlow Slides
- VexFlow Connectors
- VexFlow Lines
- VS Code Extension
- RuleFlow Directory Tree
- VexFlow Background Context
- VexFlow Tempo
- VexFlow Benchmark
- ICN Reducer Agent
- RuleFlow Pretty Printer
- RuleFlow LLM Selector
- midi2txt Original
- Happy Birthday Sheet Image
- Shared Web Helpers
- VexFlow Dots
- RuleFlow Recent Projects
- RuleFlow Modal Dialog
- midi2txtV2 Script
- midi2txtV3 Script
- midi2txtV4 Script
- midi2txt2 Script
- midi2txt3 Script
- VexFlow Hairpins
- VexFlow Volta
- FlowLang Package
- midi2txt4 Script
- midi2txtV1-1 Script
- VexFlow Ornaments
- VexFlow Vibrato
- VexFlow Glyph Lines
- icat Core
- RuleFlow Studio App
- Wolfram Rendering
- Game of Life Example
- Happy Birthday Data Files
- Concat Stop Exception
- RuleFlow Bash Script
- Do-Re-Mi-Fa Files
- Feliz Navidad Data
- Fur Elise Data
- Zelda Fairy Fountain Data
- Super Mario Data
- musicic Project Config
- RuleFlow Project Config

## God Nodes (most connected - your core abstractions)
1. `g` - 109 edges
2. `be` - 82 edges
3. `Cell` - 74 edges
4. `tt` - 72 edges
5. `vt` - 69 edges
6. `zt` - 67 edges
7. `ae` - 64 edges
8. `Ut` - 62 edges
9. `xe` - 56 edges
10. `vi` - 50 edges

## Surprising Connections (you probably didn't know these)
- `Melody: G G A G C B | G G A G D C | G G G E C B A | F F E C D C (8 measures)` --semantically_similar_to--> `Happy Birthday to You melody notebook`  [INFERRED] [semantically similar]
  2026/music sheets/happy birthday.png → 2026/musicic/notebooks/happy-birthday-to-you-melody.md
- `RuleFlow framework (Sessie algorithms, Causal Networks)` --conceptually_related_to--> `ICN paper (Caviness et al., SIMULTECH 2025)`  [AMBIGUOUS]
  2026/reference/RuleFlow-main/README.md → .claude/agents/icn-reducer.md
- `Two-point line rule (only constant or +/-1 step)` --semantically_similar_to--> `Interpolating fits warning`  [INFERRED] [semantically similar]
  2026/musicic/docs/concat-vs-reduce.html → .claude/agents/icn-reducer.md
- `MIDI tick quantization (455->32, 341->24 ...)` --semantically_similar_to--> `Duration 2^a*3^b lattice normalization`  [INFERRED] [semantically similar]
  2026/musicic/README.md → .claude/agents/icn-reducer.md
- `Test MIDI-to-text report: 1 track, 6 notes (B3/F4/G4)` --conceptually_related_to--> `Experiment 1: 45-note C-major scale exercise, decimal {start,0.474,pitch}`  [AMBIGUOUS]
  Archive/test.txt → archive 2/Experiments/txt/experiment1.txt

## Import Cycles
- None detected.

## Hyperedges (group relationships)
- **IC reducer lineage: ReduceSetList -> reduce.py -> concat.py** — _claude_agents_icn_reducer_reducesetlist, 2026_musicic_musicic_core_reduce, 2026_musicic_musicic_core_concat, 2026_musicic_docs_concat_vs_reduce, 2026_musicic_docs_concatenation_examples [EXTRACTED 1.00]
- **musIC web UI pages** — 2026_musicic_musicic_web_index, 2026_musicic_musicic_web_concatenate, 2026_musicic_musicic_web_notebook, 2026_musicic_musicic_web_sheet [EXTRACTED 1.00]
- **Picture -> set file -> score transcription pipeline** — 2026_musicic_docs_transcribing, 2026_musicic_readme_set_file, 2026_musicic_scores_readme, 2026_musicic_musicic_core_setfile, 2026_musicic_docs_transcribing_bar_length_check [INFERRED 0.85]
- **Joy to the World MIDI-dump format iterations** — archive_jjtw2alpha_jjtw2alpha, archive_jjtw3alpha_jjtw3alpha, archive_jttw2_jttw2, archive_jttw3_jttw3, archive_jttw4_jttw4, archive_jttw4alpha_jttw4alpha, archive_jttw6_jttw6, archive_jttw7_jttw7, archive_jttw8_jttw8, archive_2_joy_to_the_world_joy_to_the_world, archive_2_joy_to_the_world_flute_joy_to_the_world_flute [INFERRED 0.85]
- **Experiment 1 scale exercise variants** — archive_2_experiments_txt_experiment1_experiment1, archive_experiment1_trial_experiment1_trial, archive_2_experiments_txt_experiment1_1_experiment1_1, archive_2_experiment1_2_experiment1_2, archive_2_experiment1_trial3_experiment1_trial3, archive_2_experiment2_experiment2 [INFERRED 0.85]
- **Happy Birthday arrangement variants** — archive_2_happy_birthday_to_you_happy_birthday_to_you, archive_2_happybirthdayinc_happybirthdayinc, archive_2_happybirthdayinc2_happybirthdayinc2, archive_2_happybirthdayinc4_happybirthdayinc4 [INFERRED 0.85]
- **concat.py reduction pipeline** — 2026_musicic_docs_concat_vs_reduce_atom__start_row__step__count_, 2026_musicic_docs_concat_vs_reduce_best_block_scoring___best_block_, 2026_musicic_docs_concat_vs_reduce_bidirectional_block_extension___extend_, 2026_musicic_docs_concat_vs_reduce_reducing_flanking_rows_from_scratch, 2026_musicic_docs_concat_vs_reduce_expand_and_verify_fallback [EXTRACTED 1.00]
- **ICN applied to music** — 2026_reference_icn_paper_indexed_concatenation_notation, 2026_complete_experiments_happy_birthday_to_you_ic___indexed_concatenation_form, 2026_complete_experiments_happy_birthday_to_you_happy_birthday_melody_set__25_pitch_duration_sets_, 2026_complete_experiments_happy_birthday_to_you_levels_of_concatenation, 2026_reference_icn_paper_prospects__ic_for_musical_scores_and_dna [INFERRED 0.85]
- **Experiment 1 artifacts** — archive_2_experiment1_set_experiment_1_note_set__mathematica_output_, archive_2_experiments_midi_experiment1_experiment_1_score, archive_2_experiments_midi_experiment1_trial1_experiment_1_trial_1_score [INFERRED 0.85]

## Communities (148 total, 86 thin omitted)

### Community 0 - "RuleFlow Vec Cells"
Cohesion: 0.04
Nodes (8): Cell, _retrieve_bytes(), TrieVec, Vec, seq(), space(), TestListVec, TestTrieVec

### Community 1 - "Notebook Page UI"
Cohesion: 0.08
Nodes (79): addCell(), B, baseColumns(), bookMenu(), brackets(), changeOrder(), check(), checkAll() (+71 more)

### Community 2 - "concat.py Reducer Design"
Cohesion: 0.05
Nodes (44): Best Block Scoring (_best_block), Bidirectional Block Extension (_extend), _basis_key(), _basis_value(), Column, _const(), _entry(), _ev() (+36 more)

### Community 3 - "Server API Endpoints"
Cohesion: 0.06
Nodes (43): to_python(), bench(), BenchReq, _check_order(), compose(), _compose_set(), ComposeReq, concatenate_page() (+35 more)

### Community 4 - "RuleFlow Core Helpers"
Cohesion: 0.04
Nodes (3): Plugin, TextArea, P

### Community 5 - "RuleFlow Delta Engine"
Cohesion: 0.05
Nodes (10): DeltaSpace, DeltaSpaces, Rule, RuleMatch, RuleSet, SpaceState, SpaceState2D, SpaceStateGraph (+2 more)

### Community 6 - "Levels of Concatenation"
Cohesion: 0.07
Nodes (30): _add(), _block(), _climb(), _const(), cuttings(), _degree_in(), find(), _fit() (+22 more)

### Community 8 - "Workbench Page UI"
Cohesion: 0.10
Nodes (50): addNote(), adopt(), bench(), CODE_Q, colorFor(), copyLiteral(), deleteSelected(), drawLiteral() (+42 more)

### Community 9 - "IC Expression Rendering"
Cohesion: 0.07
Nodes (32): _bindings(), _blocks_order(), walk(), _emit(), _evaluate(), expand(), expand_text(), ICError (+24 more)

### Community 10 - "RuleFlow Music Plugins"
Cohesion: 0.06
Nodes (5): EventCausalityGraph, get_extended_scale(), P, P, P

### Community 11 - "icn.py EDSL Tool"
Cohesion: 0.07
Nodes (31): _binom(), _check(), _cost(), _count(), expand(), expand_all(), _fact(), fit() (+23 more)

### Community 12 - "VexFlow Metrics"
Cohesion: 0.07
Nodes (5): g, oi(), Te, u(), Z

### Community 13 - "reduce.py Port"
Cohesion: 0.10
Nodes (26): Expand-and-Verify Fallback, Skeleton Matching (non-1 integers to 0), _leaf_exprs(), _reindex(), _subs(), IC, Iter, _absorb_right() (+18 more)

### Community 15 - "Concat vs Reduce Comparison"
Cohesion: 0.09
Nodes (20): cell(), main(), numbers(), run(), _run(), sets(), reference_set(), rows() (+12 more)

### Community 16 - "Note Encoders"
Cohesion: 0.10
Nodes (21): decode(), _degree(), enc_contour(), enc_degree(), enc_edsl_onset(), enc_edsl_pitch(), enc_interval(), enc_ioi() (+13 more)

### Community 18 - "FlowLang Parser"
Cohesion: 0.06
Nodes (10): str_to_num(), sss_decoder(), wolfram_numbering_scheme(), decode_directive(), FlowLangParser(), FlowLangTransformer, parse_part(), import_directive() (+2 more)

### Community 20 - "RuleFlow Space States"
Cohesion: 0.09
Nodes (9): DeltaCell, SpaceState1D, BaseRule, DeletionRule, InsertionRule, OverwriteRule, ReverseRule, ShiftingRule (+1 more)

### Community 23 - "VexFlow Bounding Geometry"
Cohesion: 0.08
Nodes (3): D, E, q

### Community 25 - "FlowLang Interpreter"
Cohesion: 0.08
Nodes (13): Selector, Target, FlowLang, get_mem(), interpret_directives(), interpret_instructions(), interpret_selector(), interpret_target() (+5 more)

### Community 26 - "Notebook File Format"
Cohesion: 0.10
Nodes (23): _by_bar(), _column_words(), columns(), columns_text(), _depends(), evaluate(), _is_set(), level_items() (+15 more)

### Community 28 - "Set File Format"
Cohesion: 0.08
Nodes (19): _as_int(), _coerce_rows(), from_noteset(), key_text(), parse_key(), _positive_int(), SetFileError, _fmt() (+11 more)

### Community 31 - "Key Signatures"
Cohesion: 0.08
Nodes (10): BarAccidentals, guess_key(), Key, _octave_for(), Note, pitch_name(), decompose(), layout() (+2 more)

### Community 34 - "FlowLang Explore Plugin"
Cohesion: 0.09
Nodes (4): is_infinity(), FlowLangBase, P, reset_highlighted()

### Community 35 - "ICN Paper Concepts"
Cohesion: 0.07
Nodes (33): Happy Birthday Melody Set (25 pitch-duration sets), Happy Birthday to You Melody Level Report, iC[] Indexed Concatenation Form, Levels of Concatenation, Pitch-Duration Set Encoding, Atom (start row, step, count), A Selection of Networks (Figures 4 and 5), Campbell 1971 Comparative Survey of Programming Languages (+25 more)

### Community 36 - "VexFlow Canvas Paths"
Cohesion: 0.19
Nodes (3): ci(), st(), b()

### Community 37 - "VexFlow Module Root"
Cohesion: 0.07
Nodes (9): a(), Ee, ei, hi, _i, lookup(), measureImpl(), xt() (+1 more)

### Community 40 - "RuleFlow Vec Caching"
Cohesion: 0.06
Nodes (10): enable_bytes_cache(), retrieve_bytes(), enable_pattern_cache(), finditer(), neutralize(), NullByteArray, _retrieve_pattern(), set_regex_backend() (+2 more)

### Community 42 - "Sequence Finding (findseq)"
Cohesion: 0.12
Nodes (15): _expr(), _shortest(), num(), _to_tree(), _uses(), _as_expr(), _constant(), _crecur() (+7 more)

### Community 44 - "concat.py Block Search"
Cohesion: 0.11
Nodes (16): _atom_tokens(), _best_block(), _best_tokens(), Block, _block_tree(), _cut_tokens(), _diff(), _from_tree() (+8 more)

### Community 45 - "Set File Checking"
Cohesion: 0.11
Nodes (12): check(), describe(), _main(), SetFile, _spelled(), Warning_, _adopt_setfile(), library() (+4 more)

### Community 48 - "New Reducer Docs"
Cohesion: 0.10
Nodes (21): A New Reducer for Indexed Concatenation (concat vs reduce report), A New Reducer for Indexed Concatenation, Atom (start row, step, count), Leaves metric (rows written), ReduceSetList (SSSiCv102.wl), Reference Sets 2-6 Benchmark, Row-level extension (k=0,-1,...), Concatenation examples (+13 more)

### Community 52 - "MIDI Import Quantize"
Cohesion: 0.12
Nodes (8): _parse_key(), read_midi(), _tempo_and_meta(), lattice(), quantize(), QuantizeReport, Snap, snap_duration()

### Community 53 - "Concatenate Page UI"
Cohesion: 0.23
Nodes (23): answer(), C, copyResult(), countRows(), draw(), drawLevels(), drawMap(), fmt() (+15 more)

### Community 55 - "Transcription Rules"
Cohesion: 0.13
Nodes (19): Transcribing a picture of a score, Pickup written as leading rest, Workbench page (index.html), from IC notebook, Happy Birthday to You melody notebook, Happy Birthday to You (piece), Levels of concatenation (notebook levels), musIC README (+11 more)

### Community 57 - "VexFlow Accidental Apply"
Cohesion: 0.19
Nodes (3): fi, p(), w()

### Community 58 - "VexFlow Renderer Contexts"
Cohesion: 0.12
Nodes (3): n(), o(), r()

### Community 59 - "VexFlow Rest Alignment"
Cohesion: 0.10
Nodes (3): l(), pe, ti

### Community 61 - "concat.py Atoms Bodies"
Cohesion: 0.14
Nodes (9): Atom, Body, _candidates(), _columns_for(), _extend(), _feed(), _grow(), _period_rows() (+1 more)

### Community 64 - "Experiment 1 Variants"
Cohesion: 0.10
Nodes (21): Experiment 1-2: same 45-note scale, halved duration 91/384, Experiment 1 trial3: same 45-note scale, durations in ticks (455), Experiment 2: experiment-1 scale extended to ~47 notes, Experiment 3: ascending diatonic scale across full MIDI range from pitch 12, ~68 notes, Experiment 4: repeated C4 with geometrically halving durations (rhythmic subdivision test), 14 notes, Experiment 1-1: 45-note scale exercise, rational times, dur 91/192, Experiment 1: 45-note C-major scale exercise, decimal {start,0.474,pitch}, Joy to the World (flute arrangement): ~190 notes, rational {start,ticks,pitch} (+13 more)

### Community 65 - "Sheet Page UI"
Cohesion: 0.24
Nodes (18): copySet(), drawSet(), drawSheet(), hover(), noteInfo(), openLibrary(), openMidi(), openSetFile() (+10 more)

### Community 66 - "VexFlow Ties Curves"
Cohesion: 0.14
Nodes (4): gt(), y(), h(), ui()

### Community 69 - "RuleFlow Signals"
Cohesion: 0.11
Nodes (3): Signal, DataTable, RulesetDashboard

### Community 76 - "MIDI Export"
Cohesion: 0.13
Nodes (8): _meta_text(), to_midi(), duration_name(), export_midi(), export_set(), _get(), naming(), notebooks_list()

### Community 77 - "Server Concat Routes"
Cohesion: 0.17
Nodes (11): _concat_both(), relabel(), _concat_form(), _concat_parts(), _concat_result(), _concat_rows(), concatenate(), concatenate_stream() (+3 more)

### Community 81 - "RuleFlow Docs"
Cohesion: 0.15
Nodes (14): RuleFlow developer docs README, RuleFlow MIT License, RuleFlow README, RuleFlow framework (Sessie algorithms, Causal Networks), RuleFlow Studio Plugin Development Guide, core.engine.Flow engine, studio.model.Plugin base class, RuleFlow Studio (Textual MVC) (+6 more)

### Community 83 - "Melody Metrics"
Cohesion: 0.22
Nodes (6): _best_period(), _genericize(), _longest_repeat(), measure(), Metrics, _runs()

### Community 87 - "midi2txt5 Script"
Cohesion: 0.23
Nodes (6): generate_minimal_output(), main(), parse_midi_file(), generate_minimal_output(), main(), parse_midi_file()

### Community 93 - "VS Code Extension"
Cohesion: 0.18
Nodes (10): categories, contributes, grammars, languages, description, displayName, engines, vscode (+2 more)

### Community 98 - "ICN Reducer Agent"
Cohesion: 0.24
Nodes (9): Encoder registry (triple, pair, interval, ioi, contour, edsl_pitch ...), musIC music-set workbench, icn-reducer agent definition, Only adjacent repetition is detected (known gap), Edge Difference Set List (EDSL), ICN paper (Caviness et al., SIMULTECH 2025), icn.py reducer tool, Indexed Concatenation Notation (ICN) (+1 more)

### Community 101 - "midi2txt Original"
Cohesion: 0.29
Nodes (4): generate_report(), main(), note_name(), parse_midi_file()

### Community 102 - "Happy Birthday Sheet Image"
Cohesion: 0.28
Nodes (9): Time signature 3/4, Bass accompaniment: quarter rest + half-note chords per measure, Key: C major, Chord symbols C - G7 - C, Patty Hill and Mildred J. Hill (composers), Measure 5 bass chord with sharp (likely C7 / secondary dominant), Melody: G G A G C B | G G A G D C | G G G E C B A | F F E C D C (8 measures), Piano grand staff (treble melody, bass chords) (+1 more)

### Community 103 - "Shared Web Helpers"
Cohesion: 0.31
Nodes (6): audio(), el(), engrave(), playing, playNotes(), stopAll()

### Community 108 - "midi2txtV2 Script"
Cohesion: 0.36
Nodes (4): float_to_fraction(), generate_minimal_output(), main(), parse_midi_file()

### Community 109 - "midi2txtV3 Script"
Cohesion: 0.36
Nodes (4): float_to_fraction(), generate_minimal_output(), main(), parse_midi_file()

### Community 110 - "midi2txtV4 Script"
Cohesion: 0.36
Nodes (4): float_to_fraction(), generate_minimal_output(), main(), parse_midi_file()

### Community 111 - "midi2txt2 Script"
Cohesion: 0.36
Nodes (4): generate_minimal_output(), main(), note_name(), parse_midi_file()

### Community 112 - "midi2txt3 Script"
Cohesion: 0.36
Nodes (4): generate_minimal_output(), main(), note_name(), parse_midi_file()

### Community 116 - "FlowLang Package"
Cohesion: 0.33
Nodes (5): devDependencies, tree-sitter-cli, name, version, tree-sitter-cli

### Community 117 - "midi2txt4 Script"
Cohesion: 0.47
Nodes (3): generate_minimal_output(), main(), parse_midi_file()

### Community 118 - "midi2txtV1-1 Script"
Cohesion: 0.47
Nodes (3): generate_minimal_output(), main(), parse_midi_file()

### Community 125 - "Wolfram Rendering"
Cohesion: 0.67
Nodes (3): render_wl(), go(), leaf()

### Community 129 - "Happy Birthday Data Files"
Cohesion: 0.50
Nodes (4): Happy Birthday to You (in D/A): 1 track, ~25 notes, Happy Birthday in C v2: adds C4 bass pulses, ~40 notes, Happy Birthday in C v4: ~28 notes, Happy Birthday in C: 1 track, ~31 notes

## Ambiguous Edges - Review These
- `concat.py` → `Happy Birthday Melody Set (25 pitch-duration sets)`  [AMBIGUOUS]
  2026/complete experiments/Happy Birthday to You.pdf · relation: conceptually_related_to
- `draftpaper (raw MIDI triples and quantized pair set)` → `Happy Birthday to You (piece)`  [AMBIGUOUS]
  draftpaper.md · relation: conceptually_related_to
- `ICN paper (Caviness et al., SIMULTECH 2025)` → `RuleFlow framework (Sessie algorithms, Causal Networks)`  [AMBIGUOUS]
  2026/reference/RuleFlow-main/README.md · relation: conceptually_related_to
- `Test MIDI-to-text report: 1 track, 6 notes (B3/F4/G4)` → `Experiment 1: 45-note C-major scale exercise, decimal {start,0.474,pitch}`  [AMBIGUOUS]
  Archive/test.txt · relation: conceptually_related_to
- `Bass accompaniment: quarter rest + half-note chords per measure` → `Measure 5 bass chord with sharp (likely C7 / secondary dominant)`  [AMBIGUOUS]
  2026/music sheets/happy birthday.png · relation: references

## Knowledge Gaps
- **55 isolated node(s):** `musicic`, `ruleflow`, `tree-sitter-cli`, `RuleFlow MIT License`, `RuleFlow README` (+50 more)
  These have ≤1 connection - possible missing edges or undocumented components. (Counts symbols only; 1135 node(s) total have ≤1 connection when file, concept and rationale nodes are included.)
- **86 thin communities (<3 nodes) omitted from report** — run `graphify query` to explore isolated nodes.

## Suggested Questions
_Questions this graph is uniquely positioned to answer:_

- **Why does `Cell` connect `RuleFlow Vec Cells` to `FlowLang Explore Plugin`, `RuleFlow Core Helpers`, `RuleFlow Delta Engine`, `RuleFlow Vec Caching`, `RuleFlow Space States`, `FlowLang Interpreter`?**
  _High betweenness centrality (0.032) - this node is a cross-community bridge._
- **Are the 30 inferred relationships involving `Cell` (e.g. with `enable_bytes_cache()` and `_retrieve_bytes()`) actually correct?**
  _`Cell` has 30 INFERRED edges - model-reasoned connections that need verification._
- **What connects `musicic`, `ruleflow`, `tree-sitter-cli` to the rest of the system?**
  _55 weakly-connected nodes found - possible documentation gaps or missing edges._
- **Should `RuleFlow Vec Cells` be split into smaller, more focused modules?**
  _Cohesion score 0.036231884057971016 - nodes in this community are weakly interconnected._
- **Why does `Workbench page (index.html)` connect `Transcription Rules` to `Sheet Page UI`, `Notebook Page UI`, `Server API Endpoints`, `Concatenate Page UI`?**
  _High betweenness centrality (0.022) - this node is a cross-community bridge._

### Low-confidence Hints
_AMBIGUOUS edges — the extractor was unsure. Verify before acting on these._

- **What is the exact relationship between `concat.py` and `Happy Birthday Melody Set (25 pitch-duration sets)`?**
  _Edge tagged AMBIGUOUS (relation: conceptually_related_to) - confidence is low._
- **What is the exact relationship between `draftpaper (raw MIDI triples and quantized pair set)` and `Happy Birthday to You (piece)`?**
  _Edge tagged AMBIGUOUS (relation: conceptually_related_to) - confidence is low._