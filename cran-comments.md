## Test environments
* local Windows 10 install, 4.5.1 (2025-06-13 ucrt)
* travis.ci on github
* win-builder  R Under development (unstable) (2024-09-25 r87194 ucrt)

## Comments
This is a major update, adding a system for constructing and rendering matrix expressions, operations and equations in LaTeX

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 


# matlib 1.0.1

- Rename vignettes to put them in order
- Add `papers/matlib-useR-2016.pdf` to avoid bad URL
- Consolidate options for `print.latexMatrix`
- Fix bug in `print.latexMatrix(sparse=TRUE)`
- `plotEqn()` gains a `...` to pass other graphical parameters
- added `coffee` data (modified from `spida2`)
- Improve documentation of `latexMatrix()` and it's `print()` method to more clearly indicate how to set global options.



## Reverse dependencies

> devtools::revdep()
 [1] "biplotbootGUI"     "DIDmultiplegtDYN"  "dymo"              "ETRep"             "hyperoverlap"      "IOLS"             
 [7] "MVSKmod"           "panelSUR"          "RMCDA"             "smallstuff"        "SurprisalAnalysis" "VIRF"

## revdepcheck results

We checked 12 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* dymo
  checking whether package 'dymo' can be installed ... WARNING



