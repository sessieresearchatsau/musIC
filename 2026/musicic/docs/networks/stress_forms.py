"""Made-up sets for finding where concat.py stops short, in the style of the
paper's networks: each is an IC form, expanded to rows, handed to
concatenate, and the result measured against the form it came from.

Some forms are deliberately beyond what concat.py does; network_tests.py
--stress reports which.
"""

STRESS = {
    # counts and limits as functions of the outer index
    "quad-count":     "{€(k,1,6)[{k, k+1}, €(k^2)[{1}]]}",
    "exp-count":      "{€(k,1,6)[{0}, €(2^k)[{1}]]}",
    "down-count":     "{€(k,1,8)[€(k)[{1}], €(9-k)[{2}]]}",
    "start-limit":    "{€(k,1,6)[€(j,k,6)[{j, 2*j}]]}",
    "late-start":     "{€(k,1,7)[{k}, €(k-2)[{1, k}], {}]}",
    # nesting
    "cantor":         "{€(d,1,8)[€(n,1,d)[{n, d-n+1}]]}",
    "triangle":       "{€(k,1,7)[€(j,1,k)[{j, k}]]}",
    "three-deep":     "{€(n,1,5)[{n}, €(i,1,n)[{i, n}, €(i)[{1}]]]}",
    "four-deep":      "{€(a,1,3)[€(b,1,a)[€(c,1,b)[{a, b, c}]]]}",
    "body-period":    "{€(k,1,6)[{k}, €(j,1,3)[€(k)[{1}], {2, j+k}]]}",
    "two-blocks":     "{€(k,1,5)[{1, k}], €(k,1,6)[€(k)[{2}], {3}]}",
    # the paper's own ideas, harder
    "optional-piece": "{€(n,1,6)[€(m,0,1)[{1, 2*m+n}, €(n)[{1}], €(m)[{2, n}], {}]]}",
    "row-width":      "{€(k,1,6)[{€(k)[1]}, {2}]}",
    "shifted-row":    "{€(k,1,6)[{€(k-1)[0], k, €(6-k)[0]}]}",
    # music-like: a phrase transposed, and one cut short
    "transpose":      "{€(k,0,3)[{60+2*k,32}, {62+2*k,32}, {64+2*k,64}]}",
    "cut-short":      "{€(k,1,4)[{60,16}, {62,16}, {64,32}], {60,16}, {62,16}}",
    "noise":          "{€(k,1,5)[{1, k}, {2, 2*k}], {9, 9}, €(k,6,10)[{1, k}, {2, 2*k}]}",
}
