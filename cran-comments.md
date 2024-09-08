## Test environments
* local Windows 10 install, 4.4.1 (2024-06-14 ucrt) 
* travis.ci on github
* win-builder  R Under development (unstable) (2024-09-07 r87105 ucrt)

## Comments
This is a major update, adding a system for constructing and rendering matrix expressions, operations and equations in LaTeX

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 


# matlib 1.0.0

- added `latexMatrix()` to create a symbolic matrix
- `latexMatrix()` made more general to handle much wider variety of cases, with LaTeX output [JF]
- this is fleshed out with an `Eqn()` wrapper to work with other LaTeX-generating code in the package [PC]
- added a vignette, `inv-3d` illustrating linear transformations and matrix inverse in 3D using `rgl`
- `latexMatrix()` gets operators for matrix arithmetic (`+,-, %*%`) and operators (`t()`)
- added vignette, `latex-equations.Rmd` illustrating the variety of functions in the package for constructing matrix equations in LaTeX
- added functions `matmult()`, `matsum()`, `matpower()` similar to the operators, but providing `simplify = TRUE`, `as.numeric = TRUE`
- `Eqn()` gains a `preview` argument, rendering a LaTeX matrix expression in a Viewer pane.
- `latexMatrix()` now allows row/column names to be displayed with a matrix.


## Reverse dependencies

> devtools::revdep()
[1] "biplotbootGUI"    "DIDmultiplegtDYN" "dymo"             "hyperoverlap"    
[5] "IOLS"             "panelSUR"         "smallstuff"       "VIRF"  

## revdepcheck results

We checked 9 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems




