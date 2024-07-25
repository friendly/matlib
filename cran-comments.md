## Test environments
* local Windows 10 install, 4.4.1 (2024-06-14 ucrt) 
* travis.ci on github
* win-builder  R Under development (unstable) (2024-07-20 r86909 ucrt)

## Comments
This is a modest update, fixing bugs and adding some new features 

## R CMD check results
There were no ERRORs or WARNINGs.  


# matlib 0.9.8

- added `circle()` for drawing circles in diagrams
- `vectors()` now handles 0-length vectors more gracefully #50.
- `matrix2latex()` generalized to allow different bracket types and optionally show the size of the matrix.

# matlib 0.9.7

- `inv()` and aliases now return invisible matrix when `verbose = TRUE` to match behaviour of related functions (e.g., `gaussianElimination()`)
- `GramSchmidt()` gains an argument, `omit_zero_columns` to control whether all-zero columns are retained in the output (issue #48) [Thx: @ggrothendieck]
- Merge pull request #49 from JF to retain column names in `GramSchmidt()`.


## Reverse dependencies

> devtools::revdep()
[1] "AquaBPsim"     "biplotbootGUI" "dymo"          "hyperoverlap"  "IOLS"          "MIIPW"        
[7] "smallstuff"    "VIRF"

## revdepcheck results

We checked 9 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* MIIPW (NA)

I have contacted the package maintainer, Atanu Bhattacharjee <atanustat at gmail.com> regarding this problem.
The error loading this package has nothing to do with matlib.



