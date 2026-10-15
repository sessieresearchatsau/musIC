# concat.py on the networks of the ICN paper

The test sets here are the networks of `reference/Indexed-Concatenation-Notation-Extended.2.nb`:
Fig. 1 (section 2), Fig. 3 (section 7, the nested pentagons), Fig. 4 (both summarizations), and
Fig. 5 (a)–(k) and Fig. 6 (a)–(f) (section 8). Each network is its edge difference set list
(EDSL): one row per vertex, the differences to the vertices it points to.

| file | what it is |
|---|---|
| `rows.json` | each network's rows, expanded by Wolfram itself with the `iC` rules of `SSSiCv102.wl` |
| `paper_forms.py` | the paper's IC form for each, in ic.py syntax; each expands to exactly its rows |
| `stress_forms.py` | made-up forms, in the paper's style, for finding where concat.py stops short |

Run `uv run python network_tests.py` (the networks against the paper's forms) and
`uv run python compare_reduce.py` (concat.py and reduce.py on the same sets).

## How the sizes are counted

- **rows**: rows written down (`concat.leaves`). A row is 1; an IC counts the rows of its body.
- **numbers**: numbers and index names written, iterators included (`concat.written`).

concat.py keeps the form with the fewest rows, then the fewest numbers. A result is *ok* when it
is lossless and neither count is larger than the paper's. It is *rows-* when it has fewer rows but
more numbers.

## What concat.py could not do, and what changed

The search finds one level of blocks over *atoms* (a row, a run, a progression). That covers a
block whose period is a fixed list of atoms, `€k[{1,1,2,2}, …, €(2k)[{1,2k+8}], {}]`. It did not
cover most of the paper:

| the paper writes | network | before |
|---|---|---|
| an inner block with a body of several rows and a count in k: `€k[{1,1},{1,2k}]` | 6(b), 6(c), 4 | no block found (6b: 168 rows) |
| a pattern inside a block's body: `€(k,1,4)[€n[{1}], {1,5n+5+k}]` | 3, 5(e), 5(h), 5(j) | body left flat |
| a block of blocks whose early periods are degenerate: `€n[€(i,1,n)[€i[…]]]` | 6(d) | 6 rows for 1 |
| `€(9)[…]` for a block with nothing varying | 5(b), (d), (f), (g) | `€(k,1,9)[…]` |
| 4,112 rows | 6(f) | over 60 s |

What was added (each step is described in concat.py's docstring and at the "tokens" section):

1. **Tokens.** A token is a row or a block nested to any depth. Each of its numbers is a sum of
   coefficients times keys: 1, an index, a power of an index, or b^index. Tokens of the same
   *shape* line up position by position, and their coefficients, counts and limits are fitted
   in a new outer index, the same way atoms are.
2. **Three ways to cut the rows into tokens.** First, the items of the old search's result.
   Second, exactly repeated groups of rows. Third, pieces cut before each row of one kind,
   reduced on their own. The third handles period n holding n inner blocks.
3. **Row-level extension of token periods.** A period is fitted on its clean middle and then
   tried at k = 0, -1, … against the actual rows. This absorbs periods where an inner count
   is 0 or 1, the paper's "extend it backwards" (section 7).
4. **Bodies searched again** at every depth.
5. **Optional pieces.** Two neighbouring stretches of a body that line up once a run of extra
   pieces in one of them is left out become one body over an index with two values. The extra
   pieces sit under a count of 0 in one stretch and 1 in the other. This is Fig. 4(b), which the
   paper says was made by hand:
   `€(m,0,1)[…, €(m)[{1,2}, {1,3n+6}], {3n+2,3n+3}]`.
6. **Writing.** A block with nothing varying becomes `€(n)[…]`. Each index starts where its
   body is simplest (`€(j,k,6)[{j}]`). Expressions use their shortest form (`k(k+1)/2 + j`).
   Equal numbers inside a row are written once (`{1,€3[3]}`, as in Fig. 5(h)).
7. **Speed.** `fit` results are cached, formulas evaluate in integers, and a period whose
   second copy does not line up is dropped before any fitting. The old search does the same
   work about 5× faster.

The old search still runs first, and its result stays a candidate. Every form must expand back
to the rows exactly.

## Results on the networks

| network | rows (concat / paper) | numbers | |
|---|---|---|---|
| Fig. 1 | 6 / 6 | 25 / 25 | the paper's form exactly |
| Fig. 3 | 5 / 5 | 23 / 27 | the paper's triple nesting |
| Fig. 4(a) | 9 / 16 | 58 / 76 | finds 4(b)'s form for the same network |
| Fig. 4(b) | 9 / 9 | 58 / 58 | the paper's hand-made form exactly |
| Fig. 5(a)–(e) | 1, 2, 1, 3, 3 | ≤ paper | |
| Fig. 5(f) | 5 / 7 | 16 / 16 | |
| Fig. 5(g) | 2 / 3 | 8 / 5 | rows- (`€(k,1,2)[{3-k,4-k}]`, a ±1 step) |
| Fig. 5(h) | 5 / 5 | 10 / 10 | |
| Fig. 5(i) | 2 / 2 | 11 / 15 | |
| Fig. 5(j) | 4 / 5 | 11 / 10 | rows- (`€(2)[{}]` for `{},{}`) |
| Fig. 5(k) | 2 / 2 | 11 / 15 | |
| Fig. 6(a) | 4 / 4 | 17 / 20 | |
| Fig. 6(b) | 3 / 3 | 15 / 15 | was 168 rows |
| Fig. 6(c) | 4 / 4 | 15 / 15 | was 30 |
| Fig. 6(d) | 1 / 1 | 22 / 22 | was 6 |
| Fig. 6(e) | 1 / 1 | 7 / 7 | |
| Fig. 6(f) | 4 / 4 | 29 / 31 | about 30 s; timed out before |

That is 21 of 21 at least as compact as the paper (6 before), all lossless.

Music sets were checked too: the six reference sets of `concatenation-examples.md`, and Happy
Birthday (melody and piano) and two tracks of the Super Mario theme through seven encoders each.
None got larger, 30 of 34 got smaller, and all are lossless. The two Mario tracks that ran out of
time before now finish. On Mario the rows written fell by roughly a third to a half (for example
`mario1:degree` 863 → 430, `mario0:ioi` 592 → 279).

## Where it still stops short

From `stress_forms.py`, all still lossless:

- **Optional pieces across more than two stretches.** Step 5 lines up two stretches with one
  run of extra pieces (at most 4 pieces, sharing at most 16). Three or more stretches with
  pieces switched on and off is not searched.
- **Rows whose length changes** (`row-width`: `{€(k)[1]}`; `shifted-row`: the annihilation
  operator's `{€(k-1)[0], k, €(6-k)[0]}`). A token has a fixed number of entries per position.
- **Deep nesting on very short input** (`four-deep`, 10 rows). There are too few outer periods
  to fit, so it comes out as 3 rows rather than 1.

Two-period blocks keep the original rule: at most a quarter of the entries may move, and plain
±1 steps are always allowed. Inside a body, half may move and the ±1 exemption does not apply.
That keeps coincidences such as `{2,2,2},{1,3,3} → {3-k,k+1,k+1}` out of the results.
