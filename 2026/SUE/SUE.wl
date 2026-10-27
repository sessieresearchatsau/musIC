(* ::Package:: *)

(* ============================================================================
   SUE.wl -- S.U.E, the gated level-by-level concatenation of musIC,
             written in the Wolfram Language.

   S.U.E (best-Saving blocks, built Up level by level, with the E tools of
   the euro sign) turns a list of rows of integers into one indexed
   concatenation that expands back to exactly those rows. This file follows
   musicic/core/levels.py run with gates on, at most two gates per number,
   and a changing number weighed at 0.1 of a part (the options S.U.E sets in
   musicic/core/strategies.py). It is written to be read first: each section
   below is one step of the report "How S.U.E Concatenates a Set", in the
   same order, and the names follow the Python.

     Section 0  constants
     Section 1  fitting: polynomials, and the gated forms of step 3
     Section 2  step 1, cutting the rows into level-1 pieces
     Section 3  step 2, grouping repeats into a level
     Section 4  step 4, choosing: the cheapest cover of a level, and the
                cheapest cutting
     Section 5  step 5, writing the result with IndexedConcatenate, and
                expanding it back

   The result is written with IndexedConcatenate -- the very symbol of
   SSSiCv102.wl (SSSiCv102`IndexedConcatenate), which that package formats as
   the euro sign and turns into iC under ExpandAll. A gate is an
   IndexedConcatenate inside a number, as a term of a sum:
       67 + IndexedConcatenate[10, j - 2]
   which is 67 while j - 2 <= 0 and 77 at j = 3, because iC gives a Sequence
   and Plus[67, Sequence[]] is 67.

   One thing needs care in Mathematica. A gated number must never be left as
   a lone euro: a Sequence[] in a row drops the entry, and in an iterator
   {i, 0, Sequence[]} changes the range, where the search means 0. That can
   happen even to 67 + euro: Mathematica works out an inner euro as soon as
   its own limits are known, so at i = 0 the number i + euro[-i, 1 - j] is
   already euro[0, 1 - j], the 0 gone, before j is set. So every number with
   a gate in it is written gateSum[terms...], which stays as it is until all
   its euros have expanded and then becomes Plus[...] -- and Plus[] is 0. In
   a notebook gateSum is displayed as the plain sum.

   Use:
       Get["SSSiCv102.wl"];   (* optional: for the euro-sign display *)
       Get["SUE.wl"];
       res = SUEConcatenate[{{16,67},{16,67},{32,69},{32,67}, ...}];
       res["Form"]            (* the concatenation *)
       res["Verified"]        (* True: it expands back to the rows *)
       SUEExpand[res["Form"]] (* the rows again *)

   It needs nothing else: SUEExpand expands IndexedConcatenate the way iC
   does, so the check runs with or without SSSiCv102.wl loaded. It reproduces
   the Python search, ties broken the same way, so on the sets it was tested
   on it chooses the same cutting and the same blocks. It is slower: it is
   meant for sets of a few hundred rows.
   ============================================================================ *)

(* The contexts the user works in, so that the index names below are the
   very symbols they type (i, j, k, ...), whatever packages are loaded. *)
SUE`$userContextPath = $ContextPath;

BeginPackage["SUE`"]

SUEConcatenate::usage = "SUEConcatenate[rows] concatenates a list of rows of \
integers the way S.U.E does and returns an Association: \"Form\" (the \
concatenation, a list of IndexedConcatenate items and rows), \"Levels\" (the \
parts of every level, each with the rows it covers), \"Score\", \"Rows\" \
(rows written down), \"Parts\" and \"Verified\".";

SUEExpand::usage = "SUEExpand[form] expands a concatenation written with \
IndexedConcatenate back into its rows, as iC in SSSiCv102.wl does.";

gateSum::usage = "gateSum[x, ...] is a number with gates in it: the sum of x, \
..., taken once every IndexedConcatenate among them has expanded, so a gate \
that gives nothing adds 0 and the number never vanishes.";

Begin["`Private`"]

(* Loading the file again in the same session starts clean. *)
ClearAll[SUEConcatenate, SUEExpand, gateSum];
ClearAll["SUE`Private`*"];

(* The index names: i inside a level-1 piece, then j at level 2, k at
   level 3, ... Each is the symbol the user gets by typing it, so results
   read j, not Global`j or SUE`Private`j; one that holds a value is not
   used, and Global`j is used instead. *)
$vars = Block[{$ContextPath = SUE`$userContextPath, $Context = "Global`"},
  If[ValueQ[#], Symbol["Global`" <> SymbolName[#]], #] & /@ (Symbol /@ Characters["ijklmnpqrs"])];
vI = First[$vars];

(* The head results are written with: SSSiCv102.wl's own symbol, so that its
   euro-sign display and its ExpandAll apply. Without that package loaded
   it is an inert symbol, and SUEExpand expands it alone. *)
IC = SSSiCv102`IndexedConcatenate;

(* Without SSSiCv102.wl a result still displays with the euro sign: the
   index and its first value below, the last value above, or a count above.
   SSSiCv102.wl, when loaded, clears these and puts its own in their place. *)
If[FormatValues[SSSiCv102`IndexedConcatenate] === {},
  Format[SSSiCv102`IndexedConcatenate[x___, {v_, a_, b_}]] :=
    Row[{Underoverscript["\[Euro]", Row[{v, "=", a}], b], "[", Row[{x}, ","], "]"}];
  Format[SSSiCv102`IndexedConcatenate[x___, n_]] :=
    Row[{Overscript["\[Euro]", n], "[", Row[{x}, ","], "]"}]];


(* ============================================================================
   Section 0. Constants (the table "Every constant the search uses")
   ============================================================================ *)

$maxPiece   = 12;    (* rows in a level-1 piece, unless it is a plain repeat *)
$maxPeriod  = 12;    (* parts in one repeat *)
$maxRows    = 3000;  (* above this no search is made *)
$twoMoving  = 1/3;   (* share of numbers two repeats may change *)
$movingCost = 0.1;   (* a changing number, or a gate, against one more part *)
$gatePoints = 8;     (* repeats a gated formula is tried on, at most *)
$specials   = 2;     (* gates per number, at most *)
$gates      = True;  (* S.U.E: gates are on *)


(* ============================================================================
   Section 1. Fitting a list of numbers with a formula in an index
   ============================================================================ *)

(* polyFit[vals, maxDeg]: the lowest-degree polynomial in t = 0, 1, 2, ...
   through every value, of degree at most maxDeg, as {poly in tt, degree};
   $Failed if none fits. The polynomial of degree d is fixed by the first
   d + 1 values and must then hit all the others. *)
polyFit[vals_List, maxDeg_Integer] := Module[{n = Length[vals], p},
  Do[
    p = Expand@InterpolatingPolynomial[Transpose[{Range[0, d], vals[[1 ;; d + 1]]}], tt];
    If[Table[p /. tt -> t, {t, 0, n - 1}] === vals, Return[{p, d}, Module]],
    {d, 0, Min[maxDeg, n - 1]}];
  $Failed]

(* The gates. Each is a factor in a formula, named after the index it
   reads; it becomes a euro sign only when the result is written (section 5).
     gateAfter[j, c]  = max(0, j - c)   a step after j = c
     gateBefore[j, c] = max(0, c - j)   a step before j = c
     gateOnce[j, c]   = 1 at j = c, else 0
     gateAlt[j]       = (-1)^j *)
gateValue[{"after", c_}, t_]  := Max[0, t - c];
gateValue[{"before", c_}, t_] := Max[0, c - t];
gateValue[{"once", c_}, t_]   := Boole[t == c];
gateValue[{"alt"}, t_]        := (-1)^t;

gateTerm[{"after", c_}, v_]  := gateAfter[v, c];
gateTerm[{"before", c_}, v_] := gateBefore[v, c];
gateTerm[{"once", c_}, v_]   := gateOnce[v, c];
gateTerm[{"alt"}, v_]        := gateAlt[v];

gateHeads = _gateAfter | _gateBefore | _gateOnce | _gateAlt;
gateCount[expr_] := Count[{expr}, gateHeads, Infinity];

(* uniqueSolve[A, b]: the one x with A.x == b, or $Failed when there is
   none or more than one. Exact, over the rationals. *)
uniqueSolve[a_, b_] := Module[{x},
  If[MatrixRank[a] < Length[First[a]], Return[$Failed]];
  x = Quiet@Check[LinearSolve[a, b], $Failed];
  If[x === $Failed || a.x =!= b, $Failed, x]]

(* gatedFit[pts, maxBase]: a constant or a line in t (degree <= maxBase)
   plus one or two gates through every point {t, v}, as {base coefficients,
   {{gate, coefficient}, ...}}; $Failed if none fits. Fewer gates win, then
   a constant before a line, then the gates in the order of `names`: steps
   after, steps before, one-offs, alternation. Accepted only when
     - there is a point more than there are unknowns,
     - the constant or line alone fits where every gate is 0, on at least
       degree + 2 points (not asked of alternation, which is never 0),
     - the solution is unique, and no gate's coefficient is 0. *)
gatedFit[pts_, maxBase_] := gatedFit[pts, maxBase] = Module[
  {ts = pts[[All, 1]], vs = pts[[All, 2]], lo, hi, names, combos, unknowns,
   quiet, a, sol, out = $Failed},
  {lo, hi} = MinMax[ts];
  names = Join[Table[{"after", c}, {c, lo, hi - 1}], Table[{"before", c}, {c, lo + 1, hi}],
               Table[{"once", c}, {c, ts}], {{"alt"}}];
  Do[
    combos = If[g == 1, List /@ names, Subsets[names, {2}]];
    Do[
      Do[
        unknowns = deg + 1 + g;
        If[Length[pts] < unknowns + 1, Continue[]];
        If[FreeQ[combo, "alt"],
          quiet = Select[Range[Length[ts]], Function[r, AllTrue[combo, gateValue[#, ts[[r]]] == 0 &]]];
          If[Length[quiet] < deg + 2 ||
             uniqueSolve[Table[If[e == 0, 1, t^e], {t, ts[[quiet]]}, {e, 0, deg}], vs[[quiet]]] === $Failed,
            Continue[]]];
        a = Table[Join[Table[If[e == 0, 1, t^e], {e, 0, deg}], gateValue[#, t] & /@ combo], {t, ts}];
        sol = uniqueSolve[a, vs];
        If[sol === $Failed || MemberQ[sol[[deg + 2 ;;]], 0], Continue[]];
        out = {sol[[;; deg + 1]], Transpose[{combo, sol[[deg + 2 ;;]]}]};
        Break[],
        {combo, combos}];
      If[out =!= $Failed, Break[]],
      {deg, 0, maxBase}];
    If[out =!= $Failed, Break[]],
    {g, 1, $specials}];
  out]

(* fitIn[vals, maxDeg, v]: a formula in the index v through vals
   (t = 0, 1, ...): a polynomial of degree <= maxDeg first; then, with gates
   on and 3 to 8 values, a gated form on a constant or a line. *)
fitIn[vals_List, maxDeg_, v_] := Module[{pf, gf},
  pf = polyFit[vals, maxDeg];
  If[pf =!= $Failed, Return[pf[[1]] /. tt -> v]];
  If[! $gates || ! (3 <= Length[vals] <= $gatePoints), Return[$Failed]];
  gf = gatedFit[Transpose[{Range[0, Length[vals] - 1], vals}], Min[1, maxDeg]];
  If[gf === $Failed, Return[$Failed]];
  Sum[gf[[1, e + 1]] v^e, {e, 0, Length[gf[[1]]] - 1}] +
    Total[(#[[2]] gateTerm[#[[1]], v]) & /@ gf[[2]]]]


(* ============================================================================
   Section 2. Step 1: cutting the rows into level-1 pieces
   ============================================================================ *)

(* A piece (kind "S") is rows a+1 .. b, each column a polynomial in i:
   count rows, row t = cols evaluated at i = t. A group (kind "G") is its
   kids repeated count times, its index running 0 .. count - 1. r0 and r1
   say which rows a part covers, counting from 0 as the Python does. *)

piece[rows_, a_, b_] := Module[{n = b - a, maxDeg, cols = {}, deg = 0, vals, pf},
  (* a formula must be confirmed by a row it was not fitted on, with two
     exceptions people use: a line on two rows, and a curve on three whose
     steps change by one (72, 71, 69 = 72 - i(i+1)/2) *)
  maxDeg = Which[n == 2, 1, n == 3, Min[n - 1, 2], True, Min[Max[n - 2, 0], 3]];
  Do[
    vals = rows[[a + 1 ;; b, c]];
    pf = polyFit[vals, maxDeg];
    If[pf === $Failed, Return[$Failed, Module]];
    If[n == 3 && pf[[2]] == 2 && Abs[vals[[3]] - 2 vals[[2]] + vals[[1]]] > 1,
      Return[$Failed, Module]];
    AppendTo[cols, pf[[1]] /. tt -> vI];
    deg += pf[[2]],
    {c, Length[rows[[a + 1]]]}];
  (* a part costs 1; formulas cost a little, a lone row a little more *)
  <|"kind" -> "S", "var" -> vI, "count" -> n, "cols" -> cols, "r0" -> a, "r1" -> b,
    "penalty" -> 0.1 deg + If[n == 1, 0.3, 0]|>]

usesQ[expr_, v_] := ! FreeQ[expr, v];

(* cuttings[rows, k]: the k cheapest ways to cut the rows into pieces, by
   dynamic programming over where the last piece starts. Ties keep the
   order the options were found in, as Python's stable sort does. *)
cuttings[rows_, k_] := Module[{n = Length[rows], best, opts, seg, out = {}, segs, b, i, entry},
  best = ConstantArray[{}, n + 1];
  best[[1]] = {{0., -1, -1, None}};
  Do[
    opts = {};
    Do[
      If[Length[rows[[a + 1]]] != Length[rows[[bb]]], Break[]];
      seg = piece[rows, a, bb];
      If[seg === $Failed,
        If[bb - a > $maxPiece, Break[]];
        Continue[]];
      Do[AppendTo[opts, {best[[a + 1, idx, 1]] + 1 + seg["penalty"], a, idx, seg}],
        {idx, Length[best[[a + 1]]]}];
      (* only a plain repeat goes on past $maxPiece rows *)
      If[bb - a >= $maxPiece && AnyTrue[seg["cols"], usesQ[#, vI] &], Break[]],
      {a, bb - 1, 0, -1}];
    opts = opts[[Ordering[Transpose[{opts[[All, 1]], Range[Length[opts]]}]]]];
    best[[bb + 1]] = Take[opts, UpTo[k]],
    {bb, 1, n}];
  Do[
    segs = {}; b = n; i = idx;
    While[b > 0,
      entry = best[[b + 1, i]];
      PrependTo[segs, entry[[4]]];
      {b, i} = entry[[{2, 3}]]];
    AppendTo[out, segs],
    {idx, Length[best[[n + 1]]]}];
  out]


(* ============================================================================
   Section 3. Step 2: grouping repeats into a level
   ============================================================================ *)

(* The shape of a part: what must be the same for two parts to be repeats
   of one body. Only the numbers may differ. *)
sig[x_] := If[x["kind"] === "S", {"S", Length[x["cols"]]},
  {"G", x["var"], sig /@ x["kids"]}]

(* Every number of a part, in order: its count, then its columns (a
   piece), or the numbers of each kid (a group). *)
slots[x_] := If[x["kind"] === "S", Prepend[x["cols"], x["count"]],
  Join[{x["count"]}, Sequence @@ (slots /@ x["kids"])]]

(* rebuild[x, numbers]: x's shape filled with these numbers, in slots order. *)
rebuild[x_, nums_] := Module[{pos = 0, go},
  go[y_] := If[y["kind"] === "S",
    Module[{c = nums[[++pos]]},
      <|y, "count" -> c, "cols" -> Table[nums[[++pos]], {Length[y["cols"]]}]|>],
    Module[{c = nums[[++pos]]}, <|y, "count" -> c, "kids" -> (go /@ y["kids"])|>]];
  go[x]]

(* monomials[expr, vars]: expr as an Association monomial -> coefficient,
   where a monomial is an exponent vector over vars (lower-level indices and
   gates count as variables). *)
monomials[expr_, vars_] := If[vars === {},
  If[expr === 0, <||>, <|{} -> expr|>],
  Association[CoefficientRules[expr, vars]]]

(* fitPeriods[periods, v]: one body for periods of parts lined up position
   by position, every number of every part fitted as a formula in the new
   index v. Returns {kids, moving, total} -- moving counts the numbers that
   change, and a gate once more -- or a reason it fails. *)
fitPeriods[periods_, v_] := Module[
  {q = Length[periods], p = Length[First[periods]], kids = {}, moving = 0, total = 0,
   cols, new, exprs, vars, ms, mons, vals, poly, acc, maxDeg},
  Do[If[Length[Union[sig /@ periods[[All, x]]]] != 1, Return["shape", Module]], {x, p}];
  maxDeg = If[q <= 2, 1, Min[q - 2, 2]];
  Do[
    cols = slots /@ periods[[All, x]];
    new = Table[
      exprs = cols[[All, sl]];
      vars = Union @@ (Variables /@ exprs);
      ms = monomials[#, vars] & /@ exprs;
      mons = Union @@ (Keys /@ ms);
      acc = 0;
      Do[
        vals = Lookup[#, Key[mon], 0] & /@ ms;
        poly = fitIn[vals, maxDeg, v];
        If[poly === $Failed, Return["nofit", Module]];
        total++;
        If[usesQ[poly, v], moving += 1 + gateCount[poly]];
        acc += poly Times @@ (vars^mon),
        {mon, mons}];
      If[mons === {}, total++];
      Expand[acc],
      {sl, Length[First[cols]]}];
    AppendTo[kids, rebuild[periods[[1, x]], new]],
    {x, p}];
  If[q == 2 && moving > $twoMoving total, Return["loose", Module]];
  {kids, moving, total}]

(* block[items, s, p, q, m, v]: items s+1 .. as q repeats of p parts, the
   last cut to its first m parts (m = p: whole). The missing parts of a cut
   repeat are the ones before them with a count of 0, so they vanish. *)
block[items_, s_, p_, q_, m_, v_] := Module[{L = Length[items], periods, last, got},
  periods = Table[items[[s + t p + 1 ;; s + t p + p]], {t, 0, q - 2}];
  If[s + (q - 1) p + m > L, Return["short"]];
  last = items[[s + (q - 1) p + 1 ;; s + (q - 1) p + m]];
  If[m < p, last = Join[last, rebuild[#, Join[{0}, Rest[slots[#]]]] & /@ periods[[-1, m + 1 ;;]]]];
  got = fitPeriods[Append[periods, last], v];
  If[StringQ[got], Return[got]];
  <|"kind" -> "G", "var" -> v, "count" -> q, "kids" -> got[[1]],
    "r0" -> items[[s + 1]]["r0"], "r1" -> items[[s + (q - 1) p + m]]["r1"],
    "moving" -> got[[2]]|>]


(* ============================================================================
   Section 4. Step 4: choosing
   ============================================================================ *)

(* level[items, v]: every block that fits, then the cheapest cover of the
   items, left to right: an item alone costs 1, a block 1 + 0.1 x its
   changing numbers, a block cut short 0.05 more. Missing (None) if no block
   is chosen, which ends the climb. *)
level[items_, v_] := Module[{L = Length[items], found, q, failed, qq, got, cost, back,
    out = {}, e, s0, g, chosen = False},
  found = ConstantArray[{}, L];
  Do[
    Do[
      q = 2; failed = None;
      While[s + q p <= L,
        got = block[items, s, p, q, p, v];
        If[StringQ[got],
          If[failed === None, failed = q];
          (* a gate is only taken on three repeats or more: keep going *)
          If[$gates && MemberQ[{"nofit", "loose"}, got] && q < $gatePoints, q++; Continue[]];
          Break[]];
        found[[s + 1]] = Append[found[[s + 1]], {s + q p, got, 1 + $movingCost got["moving"]}];
        q++];
      If[failed =!= None, q = failed];
      (* the last repeat cut short: at most two parts missing, at least half kept *)
      qq = If[q > 2, Max[2, q - 1], 2];
      Do[
        If[s + (qq - 1) p + m > L, Break[]];
        got = block[items, s, p, qq, m, v];
        If[StringQ[got], Continue[]];
        found[[s + 1]] = Append[found[[s + 1]], {s + (qq - 1) p + m, got, 1 + $movingCost got["moving"] + 0.05}],
        {m, Max[Floor[(p + 1)/2], p - 2, 1], p - 1}],
      {p, 1, Min[$maxPeriod, Floor[(L - s)/2]]}],
    {s, 0, L - 1}];
  (* the cheapest cover *)
  cost = ConstantArray[Infinity, L + 1]; cost[[1]] = 0.;
  back = ConstantArray[None, L + 1];
  Do[
    If[cost[[s + 1]] === Infinity, Continue[]];
    If[cost[[s + 1]] + 1 < cost[[s + 2]], cost[[s + 2]] = cost[[s + 1]] + 1; back[[s + 2]] = {s, None}];
    Do[If[cost[[s + 1]] + f[[3]] < cost[[f[[1]] + 1]],
         cost[[f[[1]] + 1]] = cost[[s + 1]] + f[[3]]; back[[f[[1]] + 1]] = {s, f[[2]]}],
      {f, found[[s + 1]]}],
    {s, 0, L - 1}];
  e = L;
  While[e > 0,
    {s0, g} = back[[e + 1]];
    If[g === None, PrependTo[out, items[[s0 + 1]]], PrependTo[out, g]; chosen = True];
    e = s0];
  If[chosen, out, None]]

(* climb[segs]: every level from one cutting, {level-1 pieces, level-2
   parts, ...}, until a level finds nothing to group. *)
climb[segs_] := Module[{levels = {segs}, nxt},
  Do[
    nxt = level[Last[levels], $vars[[d]]];
    If[nxt === None, Break[]];
    AppendTo[levels, nxt],
    {d, 2, Length[$vars]}];
  levels]

(* score: parts at the top + 0.1 x level-1 penalties + 0.1 x the changing
   numbers of every group on levels 2, 3, ... (summed in order, as Python's
   sum does, so equal scores stay equal). *)
score[levels_] := Length[Last[levels]] +
  0.1 Fold[Plus, 0, #["penalty"] & /@ First[levels]] +
  $movingCost Fold[Plus, 0, Cases[Flatten[Rest[levels]], g_ /; g["kind"] === "G" :> g["moving"]]]


(* ============================================================================
   Section 5. Step 5: writing the result, and expanding it back
   ============================================================================ *)

(* e3[x]: a number with gates in it, each gate written as an
   IndexedConcatenate whose count is the gate: it repeats the gate's
   coefficient as many times as the gate's value, and adds nothing where
   the count is 0 or less. Alternation is written (-1)^j. *)
gateLimit[gateAfter[v_, c_]]  := v - c;
gateLimit[gateBefore[v_, c_]] := c - v;
gateLimit[gateOnce[v_, c_]]   := 1 - (v - c)^2;

e3[x_] := Module[{ex = Expand[x], gs, g, co},
  gs = Union@Cases[{ex}, _gateAfter | _gateBefore | _gateOnce, Infinity];
  If[gs === {}, Return[ex /. gateAlt[v_] :> (-1)^v]];
  g = First[gs];
  co = Coefficient[ex, g];
  e3[ex - co g] + IC[e3[co], gateLimit[g]]]

(* num[x]: a number as it is written in a row or a limit. One with a gate
   in it is wrapped in gateSum (see the top of the file), so it is a sum
   whatever order Mathematica evaluates its parts in. *)
num[x_] := Module[{r = e3[x]},
  If[FreeQ[r, IC], r, gateSum @@ If[Head[r] === Plus, List @@ r, {r}]]]

(* gateSum adds up once nothing is left to expand: no IndexedConcatenate,
   and no iC or icEval still waiting for an outer index. *)
gateSum[x___] /; FreeQ[{x}, _IC | _icEval | _SSSiCv102`iC] := Plus[x];
(* displayed as the plain sum; MakeBoxes never evaluates its left side, so
   the rule above cannot rewrite it *)
MakeBoxes[gateSum[x___], fmt_] := MakeBoxes[HoldForm[Plus[x]], fmt];

(* treeOf[x]: a part written the simplest way that says what it is. *)
treeOf[x_] := Module[{uses},
  If[x["kind"] === "S",
    uses = AnyTrue[x["cols"], usesQ[#, vI] &];
    Which[
      ! uses && x["count"] === 1, num /@ x["cols"],
      ! uses, IC[num /@ x["cols"], num[x["count"]]],
      True, IC[num /@ x["cols"], {vI, 0, num[x["count"] - 1]}]],
    uses = AnyTrue[Flatten[slots /@ x["kids"]], usesQ[#, x["var"]] &];
    If[uses,
      IC[Sequence @@ (treeOf /@ x["kids"]), {x["var"], 0, num[x["count"] - 1]}],
      IC[Sequence @@ (treeOf /@ x["kids"]), num[x["count"]]]]]]

(* SUEExpand: IndexedConcatenate expanded as iC in SSSiCv102.wl expands it:
   the body tabled over the iterator and spliced into what holds it. An
   inner euro whose limits use an outer index waits until the outer one has
   set it. *)
icEval[iter : (_Integer | {_, _Integer, _Integer})] := Sequence[];
icEval[x__, iter : (_Integer | {_, _Integer, _Integer})] := Sequence @@ (Join @@ Table[{x}, iter]);
SUEExpand[form_] := form //. IC -> icEval;

(* rows written down: a row is 1, a euro the rows of its body *)
leaves[items_List] := Total[If[Head[#] === IC, leaves[Most[List @@ #]], 1] & /@ items];


(* ============================================================================
   The search
   ============================================================================ *)

SUEConcatenate[rows0_List] := Module[
  {rows = rows0, n = Length[rows0], k, best = None, bestScore = None, lv, sc, form},
  k = Which[n <= 120, 24, n <= 600, 8, True, 3];
  If[n <= $maxRows,
    Do[
      lv = climb[segs];
      sc = score[lv];
      If[bestScore === None || sc < bestScore, best = lv; bestScore = sc],
      {segs, cuttings[rows, k]}]];
  If[best === None,           (* too many rows: each row its own piece *)
    best = {Table[<|"kind" -> "S", "var" -> vI, "count" -> 1, "cols" -> rows[[t]],
      "r0" -> t - 1, "r1" -> t, "penalty" -> 0.3|>, {t, n}]}];
  form = treeOf /@ Last[best];
  <|"Form" -> form,
    "Levels" -> Map[{#["r0"], #["r1"]} &, best, {2}],
    "Score" -> bestScore,
    "Rows" -> leaves[form],
    "Parts" -> Length[form],
    "Verified" -> (SUEExpand[form] === rows)|>]

End[]
EndPackage[]
