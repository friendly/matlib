# matlib 0.6.1

- use `corner()` in vectors3d.Rd
- added `arc()` for 2D, 3D vector diagrams
- added `regvec3d()` for 2D, 3D vector diagrams representing a bivariate multiple regression model, `lm(y ~ x1 + x2)` [thx: John Fox]

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

