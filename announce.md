Dear useRs

A new release of the "matlib" package, v. 0.8.1 is now on CRAN,
http://cran.us.r-project.org/web/packages/matlib/

The package is designed to provide a collection of functions and vignettes
for teaching and learning linear algebra and multivariate statistics.

This release adds five more vignettes covering determinants and matrix inverses,
some with geometric diagrams illustrating concepts and using functions in the
package.

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
