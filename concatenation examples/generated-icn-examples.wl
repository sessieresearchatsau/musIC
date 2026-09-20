(* ICN examples for review -- all finite, no Infinity.
   Each block: the raw list of sets, the IC form, and the check that
   ExpandAll of the IC form reproduces the raw list exactly. *)

SetDirectory[NotebookDirectory[]];
Needs["SSSiCv100`"];

(* ---- A: flat, no nesting ---- *)
rawA = {{1,2}, {1}, {1,2}, {1}, {1,2}, {1}};
icA  = {IndexedConcatenate[{1, 2}, {1}, {n, 1, 3}]};
ExpandAll[icA] === rawA

(* ---- B: j inside a set -> set LENGTH grows ---- *)
rawB = {{1}, {1,2}, {1,2,3}, {1,2,3,4}};
icB  = {IndexedConcatenate[{IndexedConcatenate[j, {j, 1, n}]}, {n, 1, 4}]};
ExpandAll[icB] === rawB

(* ---- C: flat, formulas in n, no nesting ---- *)
rawC = {{1,1,2,2}, {2,2}, {1,5}, {1,1}, {1,10}, {}, {1,1,2,2}, {2,4}, {1,7}, {1,1}, {1,12}, {}, {1,1,2,2}, {2,6}, {1,9}, {1,1}, {1,14}, {}};
icC  = {IndexedConcatenate[{1, 1, 2, 2}, {2, 2*n}, {1, 2*n+3}, {1, 1}, {1, 2*n+8}, {}, {n, 1, 3}]};
ExpandAll[icC] === rawC

(* ---- D: j outside the sets -> COUNT of sets grows ---- *)
rawD = {{1,2}, {1}, {}, {1,2}, {1,2}, {1}, {}, {1,2}, {1,2}, {1,2}, {1}, {}, {1,2}, {1,2}, {1,2}, {1,2}, {1}, {}};
icD  = {IndexedConcatenate[IndexedConcatenate[{1, 2}, {j, 1, n}], {1}, {}, {n, 1, 4}]};
ExpandAll[icD] === rawD

(* ---- E: triply nested: i in j in n ---- *)
rawE = {{1}, {}, {1}, {1,2}, {}, {1}, {1,2}, {1,2,3}, {}};
icE  = {IndexedConcatenate[IndexedConcatenate[{IndexedConcatenate[i, {i, 1, j}]}, {j, 1, n}], {}, {n, 1, 3}]};
ExpandAll[icE] === rawE

(* ---- F: bare euro^(n-1); euro^0 empty -> unequal row lengths ---- *)
rawF = {{1}, {2,1}, {2,2,1}, {2,2,2,1}, {2,2,2,2,1}};
icF  = {IndexedConcatenate[{IndexedConcatenate[2, n-1], 1}, {n, 1, 5}]};
ExpandAll[icF] === rawF

(* ---- G: nested and variable-length together ---- *)
rawG = {{1}, {}, {1,1}, {2,1}, {}, {1,1,1}, {2,1,1}, {2,2,1}, {}};
icG  = {IndexedConcatenate[IndexedConcatenate[{IndexedConcatenate[2, j-1], IndexedConcatenate[1, n-j+1]}, {j, 1, n}], {}, {n, 1, 3}]};
ExpandAll[icG] === rawG

(* all seven at once *)
And @@ {ExpandAll[icA] === rawA, ExpandAll[icB] === rawB, ExpandAll[icC] === rawC, ExpandAll[icD] === rawD, ExpandAll[icE] === rawE, ExpandAll[icF] === rawF, ExpandAll[icG] === rawG}
