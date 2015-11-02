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

