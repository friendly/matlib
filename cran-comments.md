## Test environments
* local Windows 10 install, 4.5.1 (2025-06-13 ucrt)
* travis.ci on github
* win-builder  R Under development (unstable) (2025-10-03 r88899 ucrt)

## Comments
This is a moderate update, improving usability and documentation

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

This is the WARNING. It seems to relate to a change in the dymo package, rather than anything different in matlib.
I notified the package maintainer, and he is looking into it. I believe this can safely be ignored. 

* checking whether package 'dymo' can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import 'matlib::size' by 'tictoc::size' when loading 'dymo'
See 'C:/Dropbox/R/projects/matlib/revdep/checks/dymo/new/dymo.Rcheck/00install.out' for details.



