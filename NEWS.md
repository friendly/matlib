# matlib 0.8.0

- add a vignette on properties of determinants (`det-ex1`)

# matlib 0.7.3

- Changed gaussianElimination() by defining local ERO functions to make the algorithm clearer; in verbose mode, show each ERO.
- Added a draw argument to `vectors3d()` and `arrows3d()`, which defaults to TRUE. If FALSE, just returns 
  returns the "reg.length" to help in scaling.
- Optionally scale error spheres (and circles) produced by regvec3d() so that they project confidence intervals on the x1 and x2 axes.
- Small cosmetic changes to regvec3d().
- `showEqn()` and `gaussianElimination()` get `latex` arguments, to print results in LaTeX format [thx: Philo Chalmers]



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

