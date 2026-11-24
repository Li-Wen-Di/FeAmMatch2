# Release validation

Two complete two-loop cold runs started from the distributed UV and EFT `.fr` files on macOS ARM64. A separate clean copy was installed and checked using a fresh Julia depot. FORM 5.0.2, QGRAF 4.0.5 and Julia 1.10.12 were used. The release includes the same sunset header used in both cold runs. A separate FORM smoke test checked the bundled default against the original file using an exact-`D` sunset recurrence.

| Example | Finite coefficients | Additional checks |
|---|---|---|
| example1 | All 27 tree, one- and two-loop Green and physical coefficients match the paper | All three one-loop coefficients at order $\epsilon$ agree with Eq. (190) |
| example2 | All six Green and three physical coefficients match the frozen benchmark | All three physical coefficients agree with the complete published Eq. (34) |

Both benchmarks also pass the full one-loop Laurent comparison, Eq. (160) amplitude-term comparison, two-loop counterterm comparison, double-pole recursion and independent one-loop RG prediction of the double logarithms. These comparisons use symbolic zero residuals. The inferred overall two-loop counterterm provides an assembly check.

Eight Wolfram local checks and the Python helper contracts passed. The [machine-readable acceptance record](acceptance.json) identifies the tested source, toolchain and reference outputs. Full cold-run logs and independent comparison scripts are maintained as development evidence.
