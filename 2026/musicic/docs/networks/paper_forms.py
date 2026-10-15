"""The networks of Indexed-Concatenation-Notation-Extended.2.nb, as the paper
writes their EDSLs, transcribed into ic.py syntax.

rows.json holds the rows each one expands to, computed by Wolfram itself (the
iC rules of SSSiCv102.wl), so the test sets do not depend on this transcription:
tests check that every form here expands to exactly those rows.
"""
import json
from pathlib import Path

ROWS = json.loads((Path(__file__).parent / "rows.json").read_text())

# name -> (where it is in the paper, the paper's IC form)
FORMS = {
    "fig1": ("Fig. 1, the motivating network",
             "{€(k,1,18)[{1,1,2,2}, €(j,1,2)[{2,2*j}], {1,2*k+3}, {1,1}, €(2*k)[{1,2*k+8}], {}]}"),
    "fig3": ("Fig. 3, nested pentagons",
             "{€(n,1,8)[{1,5*n+4,5*n+5}, €(k,1,4)[€(i,1,n)[{1}], {1,5*n+5+k}], €(j,1,n-1)[{1}], {}]}"),
    "fig4a": ("Fig. 4, first summarization",
              "{€(n,1,7)[{1,2,3}, {3*n+1,3*n+2}, {1,2}, {3*n-1,3*n+1}, €(n-1)[€(2)[{1,2}], {3*n,3*n+1}],"
              " {3*n+2,3*n+3}, {1,2,3}, {3*n+3,3*n+4}, {1,2}, {3*n+1,3*n+3},"
              " €(n-1)[€(2)[{1,2}], {3*n+2,3*n+3}], {1,2}, {1,3*n+6}, {3*n+2,3*n+3}]}"),
    "fig4b": ("Fig. 4, second summarization (by hand)",
              "{€(n,1,7)[€(m,0,1)[{1,2,3}, {2*m+3*n+1,2*m+3*n+2}, {1,2}, {2*m+3*n-1,2*m+3*n+1},"
              " €(n-1)[€(2)[{1,2}], {2*m+3*n,2*m+3*n+1}], €(m)[{1,2}, {1,3*n+6}], {3*n+2,3*n+3}]]}"),
    "fig5a": ("Fig. 5(a)", "{€(10)[{1}]}"),
    "fig5b": ("Fig. 5(b)", "{€(5)[{1}, {}]}"),
    "fig5c": ("Fig. 5(c)", "{€(10)[{1,2}]}"),
    "fig5d": ("Fig. 5(d)", "{€(9)[{2,3}, {3}, {1,2}]}"),
    "fig5e": ("Fig. 5(e)", "{€(3)[€(k,1,3)[{2*k-1,2*k}], €(2)[{1,2}, €(2)[{}]]]}"),
    "fig5f": ("Fig. 5(f)", "{€(4)[{2,2,2}, {1,3,3}, {2}, {1,1,3}, {2}, {1,1,1}, {3}]}"),
    "fig5g": ("Fig. 5(g)", "{€(8)[{2,3}, {1,2}, {}]}"),
    "fig5h": ("Fig. 5(h)", "{€(7)[{1,6,8}, {}, {€(4)[2]}, €(2)[{1,€(3)[3]}, {}]]}"),
    "fig5i": ("Fig. 5(i)", "{€(n,0,25)[€(k,1,7)[{1,18-2*k}], €(k,8,9)[{1,1}]]}"),
    "fig5j": ("Fig. 5(j)", "{€(13)[€(2)[{1,7,8}], €(2)[{1,1,7}], {7}, {}, {}]}"),
    "fig5k": ("Fig. 5(k)", "{€(n,0,18)[€(k,1,4)[{1,12-2*k}], €(k,5,7)[{1,1}]]}"),
    "fig6a": ("Fig. 6(a)", "{€(i,1,10)[{1,1,1}, {1,1,i+1}, €(i-1)[{1,1,i+2}], {i+2,i+3}]}"),
    "fig6b": ("Fig. 6(b)", "{€(k,1,12)[{2*k-1,2*k+1}, €(k)[{1,1}, {1,2*k}]]}"),
    "fig6c": ("Fig. 6(c)", "{€(k,1,8)[€(k-1)[{}, {1,1,2,2*k}], {}, {2*k,2*k+2}]}"),
    "fig6d": ("Fig. 6(d)", "{€(n,1,5)[€(i,1,n)[€(i)[{n*(n+1)/2, n*(n+1)/2+i, n*(n+1)/2+i+1}]]]}"),
    "fig6e": ("Fig. 6(e)", "{€(k,1,9)[€(k)[{k,k+1}]]}"),
    "fig6f": ("Fig. 6(f)", "{€(k,1,10)[{2^k-1,2^k,2^k+1}, €(2^k+1)[{2^k+1}], {1},"
                           " €(j,1,2^k-1)[{1,2^k+j-1,2^k+j}]]}"),
}


def rows(name: str) -> list[tuple]:
    return [tuple(r) for r in ROWS[name]]
