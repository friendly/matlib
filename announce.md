Dear useRs

A new package, "matlib" has been under development and the latest version, 0.5.2,
will shortly be on CRAN.
http://cran.us.r-project.org/web/packages/matlib/

The package is designed to provide a collection of functions and vignettes
for teaching and learning linear algebra and multivariate statistics.

In some cases, convenience functions are provided for concepts available
elsewhere in R, but where the function call or name is not obvious.  In other
cases, functions are provided to show or demonstrate an algorithm.

The topics covered in the package include:

* Vector geometry: vector diagrams, projection 

* Determinants: minors, cofactors and expansion by cofactors

* Elementary row operations: functions for solving linear equations "manually" by the steps used in row echelon form and Gaussian elimination

* Linear equations: functions to illustrate and plot linear equations of the form $\mathbf{A x = b}$

* Gaussian elimination: functions for illustrating Gaussian elimination for solving systems of linear equations of the form
$\mathbf{A x = b}$.  These functions provide a `verbose=TRUE` argument to show the intermediate steps.

* Eigenvalues: functions to illustrate the algorithms for calculating eigenvalues and eigenvectors and the SVD

* browseVignettes("matlib") shows currently available vignettes.

The GitHub development page for the project is:
https://github.com/friendly/matlib

Comments, suggestions, issues, etc. are invited there.

best,
-Michael
