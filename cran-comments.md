## Test environments
* local Windows 7 install, 3.3.2 (2016-10-31)
* win-builder (R version 3.3.2 (2016-10-31), R Under development (unstable) (2017-02-04 r72100)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs

## Comments
This is a major release, renaming functions for consistency and adding a number of new functions
and making other functions more usable or flexible.

# matlib 0.9.0
- added `printMatEqn()` to print matrix expressions side-by-side
- prepare to release as a cumulative major version
- Phil Chalmers is now recognized officially as a package author [aut]

# matlib 0.8.3

- rename functions for consistency: `eig()` -> `Eigen()`, `point_on_line()` -> `pointOnLine()`, `power_method()` -> `powerMethod()`, `row_cofactors()` -> `rowCofactors()`, `row_minors()` -> `rowMinors()`.

- add `Det()` to compute determinants by elimination, from eigenvalues, or by minors and cofactors, with possibility of `verbose` output.
- `plotEqn3d()` gets an `axes` argument and `lit` to control lighting of the planes; `lit` solves a problem with the planes becoming indistinguishable in some rotations.
- add `svdDemo()` function to illustrate the SVD of a 3 x 3 matrix [thx: Duncan Murdoch]
- add `symMat()` to create a square symmetric matrix from a vector.
- add `angle()` to calculate angle between vectors
- `powerMethod()` gets a `keep` argument, for possible use in plotting the convergence of eigenvectors.
- add `adjoint()`, to round out methods for determinants
- add `GramSchmidt()` for the Gram-Schmidt algorithm on columns of a matrix. The existing function `gsorth()` will be deprecated and then removed.
- `gsorth()` has been deprecated.
- fixed use of MASS::fractions in gaussianElimination
- added `printMatEqn()` to print matrix expressions side-by-side

