# Approaches to concatenating a long set

One set has many indexed concatenations that expand back to it exactly. Which
one a search returns depends on three choices, and each approach is named by
them. The code is `musicic/core/strategies.py`; `compare_strategies.py` runs
every approach on every set and writes `results.json` here, which the
Concatenate page shows under **Approaches**.

## The labels

**How a block is chosen**

| | | |
|---|---|---|
| G | greedy-shortest | the shortest repeating pattern is fixed first and never revisited (ReduceSetList) |
| S | best-saving | every candidate is scored; the one that saves the most wins |
| B | bar-anchored | no piece crosses a bar line, and each bar is one part, so the levels above group bars, then phrases |
| V | variant-merging | repeats that differ by a piece still line up; where the piece is absent its count is 0 |

**How levels are built**

| | | |
|---|---|---|
| F | flat, then nested | the biggest block over the raw rows, then the search goes inside its body |
| U | up, level by level | short pieces, then groups of pieces, then groups of those |

**What € may do**

| | | |
|---|---|---|
| E1 | repeat and index | `€(i,0,1)[{69-2i, 32}]` |
| E2 | a count that comes out 0 or 1 | `€(i,0,2-3j)`: three rows at j = 0, none at j = 1 |
| E3 | a € inside a number, as a term of a sum | `€(n,0,3)[67 + €(n-2)[10]]` is 67, 67, 67, 77 |
| E4 | a € inside a row | `{1, €(3)[3]}` is `{1, 3, 3, 3}` |

E3 is new in `ic.py`. A € there adds up what it expands to and adds nothing
when it is empty. That is how Mathematica reads it: `iC` gives a `Sequence`,
and `Plus[67, Sequence[]]` is 67. Every form below was checked against
`wolframscript` with `SSSiCv102.wl`. Only `+` may join such a €, since
`2*Sequence[10,10]` is `Times[2,10,10]`.

The gates built from E2 and E3 come in three kinds, each written in the
repeat's index j:

- a **step**, `€(j-2)[d]`, which adds d once j passes 2;
- a **one-off**, `€(1-(j-2)^2)[d]`, which adds d at j = 2 only;
- an **alternation**, `(-1)^j`.

A gate is used only where three repeats or more confirm it. The search tries
gates on runs of at most 8 repeats.

**The approaches:**

| Approach | Name | What it uses |
|---|---|---|
| G·F | ReduceSetList, ported | `reduce.py` |
| S·F | Fewest rows | `concat.py` |
| S·U | Level by level, as the Notebook uses it | `levels.py`, defaults |
| B·U | Bar by bar | `levels.py`, with bar lines |
| S·U·E | Gated levels | `levels.py`, with gates |
| V·U·E | Variants | `levels.py`, with gates and variants |
| B·V·U·E | Bars and variants | `levels.py`, with all three |

The E approaches count a number that changes between repeats as 0.1 of a part,
where S·U counts it as 0.4. At 0.4 a gate never pays for its numbers.

## The corpus

There are 164 sets in all.

**Music (120 sets).** Twenty pieces, each through six encoders: `pair`,
`triple`, `interval`, `ioi`, `degree` and `contour`.

- Hand transcriptions in `scores/`:
  - Happy Birthday, three readings;
  - Frère Jacques;
  - the Ode to Joy theme.
- The MIDI the old experiments came from:
  - Happy Birthday in C;
  - Joy to the World;
  - Für Elise;
  - Zelda;
  - Feliz Navidad;
  - experiment 1;
  - Mario.
- New MIDI in `2026/midis`:
  - Frère Jacques;
  - Bach BWV 846, the upper part and both parts;
  - Pachelbel's Canon, the first 24 bars;
  - the twelve-bar blues, its bass line and its riff;
  - Mozart K. 265, the theme alone and with variations 1–2.

**Other sets (44).**

- The 6 reference sets.
- The 21 networks of the ICN paper.
- 17 stress forms.

The Ode to Joy MIDI is a humanized orchestral performance, so the theme was
written out by hand instead. Frère Jacques' MIDI is written in sixteenths, and
its times are scaled by 4. Mozart's file ends in 3/4 (variation 12), so it is
read in 2/4.

## What the corpus shows

Every result expands back exactly. Three runs time out at 180 s: the gated
approaches on the `triple` encodings of Mario, BWV 846 and the Canon, 549 to
1,224 rows each.

| | G·F | S·F | S·U | B·U | S·U·E | V·U·E | B·V·U·E |
|---|---|---|---|---|---|---|---|
| music: fewest rows (of 120) | 0 | 1 | 16 | 9 | 72 | 75 | 7 |
| music: rows written / rows in set (median) | 0.84 | 0.57 | 0.27 | 0.35 | 0.17 | 0.18 | 0.28 |
| music: numbers written / rows in set (median) | 1.77 | 1.78 | 2.79 | 2.59 | 2.96 | 3.01 | 2.66 |
| music: bar fit (mean) | 0.78 | 0.73 | 0.73 | 1.00 | 0.75 | 0.74 | 1.00 |
| networks + reference: fewest rows (of 27) | 7 | 22 | 6 | – | 7 | 7 | – |

These figures are for the latest run. They change when it is run again.

1. **Rows and numbers pull against each other.** The level approaches cut a
   melody to a quarter or a sixth of its rows. They pay for it in formulas,
   writing 2.6 to 3 numbers for every row of the set, and a `pair` row is
   itself only 2 numbers. G·F and S·F barely shorten a melody, so they win on
   numbers by default. Which form is "best" has to say which of the two it
   counts.
2. **Bar-anchoring is the cheapest structure in numbers.** Among the
   approaches that really compress, B·U writes the fewest numbers, and every
   one of its blocks respects the bar lines. On Mario (`ioi`) it writes 80 rows
   and 644 numbers, against 193 rows and 2,087 numbers for S·U. On Frère Jacques
   it finds the musical reading, four times "this bar, twice":
   `€(2)[…], €(2)[…], €(2)[…], €(2)[…]`.
3. **Gates find what the blackboard drew, and overfit where there is nothing
   to find.**
   - On Happy Birthday, S·U·E writes the whole song as one `€(j,0,3)` over its
     four phrases: 3 rows and 58 numbers, against 7 rows and 65 numbers for
     S·U. It writes the first notes as `67 + €(j-2)[10]`, phrase 3's extra note
     as a count `1 + €(1-(j-2)^2)[1]`, and alternating cadences as `(-1)^j`.
   - On Frère Jacques, which only repeats, the same approach folds bars 5–8
     with formulas like `8j²−24j+32`: 5 rows, but 111 numbers.
   - On Fig. 6(b) it does worse than S·U, 38 rows against 22.
   - At a weight of 0.1, a gate is cheap enough to be used where it explains
     nothing.
4. **Variants add little so far.** V·U·E differs from S·U·E on a few sets
   only. Ode to Joy's A and A′ differ by a transposed cadence, which a plain
   formula already covers, so they need no switch.
5. **S·F is the approach for the networks.** It writes the fewest rows on 22 of
   the 27 networks and reference sets, which is what it was tuned on. It is the
   only one that handles Fig. 6(f), 4,112 rows; the level search stops at 3,000.
6. **No approach writes a bar plainly and then repeats it.** Every level
   search cuts a bar into two-row formula pieces, such as
   `€(i,0,1)[{32, 2i+60}]` for C–D, because rows are counted first. A
   repeats-first approach is a candidate R: exact repeats first, formulas
   only inside them.

## Run it

```bash
uv run python compare_strategies.py                    # all 164 sets, about 30 min
uv run python compare_strategies.py --only bach,mozart # just these; the rest are kept
```
