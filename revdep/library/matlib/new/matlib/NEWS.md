# matlib 0.9.4

- added `xprod()` for vector cross-product
- added data-beta vignette
- fix `sprintf` warning from LU [Phil Chalmers]
- better plotting of planes in plotEqn3d, alpha=0.9

# matlib 0.9.3

- `gaussianElimination()` and friends now handle one-row or one-column matrices [suggestion of Jana Jarecki]
- improvements to `plotEqn()`

# matlib 0.9.2

- Bug fix in arrows3d.r [Thx: Douglas Whitaker]
- Bump package version
- matlib gets a hex sticker
- Incorporated a numerical tolerance in `GramSchmidt()` [John Fox]
- Improved computation of SVD [John Fox]
- now export `printMatrix()`
- now export `circle3d()` [req: Marco Scazzocchio]
- now use_revdep()

# matlib 0.9.1

- fix references to car datatsets -> carData

# matlib 0.9.0

- `showEqn()` gains a `reduced` logical to print only the unique regression equations
  when a `lm()`-type object is passed. May be combined with the `simplify` logical argument
  for further reductions
- `echelon()` gains a `reduced` logical to indicate whether the reduced or non-reduced form is computed
- `powerMethod()` gains a `plot = TRUE` logical to draw the iteration history
- added support for `'lm'` objects to `showEqn()` to show the design matrix equations
- added `verbose` option to `GramSchmidt()` and another example
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

# matlib 0.8.1

- remove inst/doc to satisfy CRAN

# matlib 0.8.0

- add a vignette on properties of determinants (`det-ex1`)
- add a vignette on evaluation of determinants (`det-ex2`)
- add vignette on matrix inverse (`inv-ex1`)
- now use travis-ci to check builds
- add vignette on matrix inverse using EROs (`inv-ex2`)
- extended use of geometric diagrams in vignettes
- add vignette on generalized inverse (`ginv`)
- added `J()` for unit vectors, matrices
- added `LU()` for LU decomposition

# matlib 0.7.3

- Changed gaussianElimination() by defining local ERO functions to make the algorithm clearer; in verbose mode, show each ERO.
- Added a draw argument to `vectors3d()` and `arrows3d()`, which defaults to TRUE. If FALSE, just returns 
  returns the "reg.length" to help in scaling.
- Optionally scale error spheres (and circles) produced by regvec3d() so that they project confidence intervals on the x1 and x2 axes.
- Small cosmetic changes to regvec3d().
- `showEqn()` and `gaussianElimination()` get `latex` arguments, to print results in LaTeX format [thx: Phil Chalmers]


# matlib 0.7.2

- added argument `error.sphere` to `plot.regvec3d()` [JF]
- remove use of `lengths()` in `corner()` to avoid R version dependency


# matlib 0.7.0

- use `corner()` in vectors3d.Rd
- added `arc()` for 2D, 3D vector diagrams to show angles between vectors
- added `regvec3d()` for 2D, 3D vector diagrams representing a bivariate multiple regression model, `lm(y ~ x1 + x2)` [thx: John Fox]
- added internal `.arrows()` to produce nice arrows in 2D `vector()` diagrams
- fixed numerous small problems in vector diagrams
- fixed some erroneous statements in vignettes
- `showEqn()` now aligns terms vertically and prints without quotes
- reversed sense of `absolute` in `points_on_line()` and clarified documentation

# matlib 0.6.0

- added `vandermode()` function
- added `vec()` convenience function to vectorize a matrix
- added `is_square_matrix()` tests
- added `power_method()`, power method for dominant eigenvector [thx: Gaston Sanchez]
- added `arrows3d()` for 3D geometric diagrams
- added `vectors3d()` for 3D geometric diagrams
- added `corner()` for 2D, 3D geometric diagrams
- added more documentation content to `man/matlib.Rd` from `README.md`


# matlib 0.5.2

- added `swp()` function
- added `vignette("gramreg")` - *Gram-Schmidt Orthogonalization and Regression*

# matlib 0.5.1

- added `len()` convenience function for Euclidean lengths
- added `plotEqn3d()` function using `rgl` to plot equations in 3 unknowns
- reorganized `matlib.R` to become a package .Rd document
- renamed `proj()` -> `Proj()` to avoid conflict with `stats::proj()`
- added `vectors()` for plotting geometric diagrams
- added vignette("linear-equations") - *Solving Linear Equations*

# matlib 0.4.1

Initial CRAN release

