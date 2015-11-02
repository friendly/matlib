# matlib
Matrix Functions for Teaching and Learning Linear Algebra and Multivariate Statistics

Version 0.4

These functions are mainly for tutorial purposes in learning matrix algebra
ideas using R. In some cases, functions are provided for concepts available
elsewhere in R, but where the function call or name is not obvious.  In other
cases, functions are provided to show or demonstrate an algorithm.

## Installation

This package can be installed to your R library directly from this repo via

     if (!require(devtools)) install.packages("devtools")
     library(devtools)
     install_github("friendly/matlib")

This installs the package from the source, so you will need to have 
R Tools installed on your system.  [R Tools for Windows](https://cran.r-project.org/bin/windows/Rtools/)
takes you to the download page for Windows.  [R Tools for Mac OS X](https://cran.r-project.org/bin/macosx/tools/)
has the required programs for Mac OS X.


Alternatively, a Windows binary (not always current) is available at http://www.psych.yorku.ca/lab/psy6140/R/matlib.zip
and can be installed via

    install.packages("http://www.psych.yorku.ca/lab/psy6140/R/matlib.zip", repos=NULL)

## Contents

1. convenience functions 

  - `tr()` - trace of a matrix
  - `R()` - rank of a matrix
  - `proj(y, X)` - projection of vector y on colunms of X

2. determinants

  - `minor()` - Minor of A[i,j]
  - `cofactor()` - Cofactor of A[i,j]
  - `row_minors()` - Row minors of A[i,]
  - `row_cofactors()` - Row cofactors of A[i,]

3. elementary row operations

  - `rowadd()` - Add multiples of rows to other rows
  - `rowmult()` - Multiply rows by constants
  - `rowswap()` - Interchange two rows of a matrix

