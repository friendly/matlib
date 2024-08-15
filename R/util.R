# The parent flag is used to search in the parent envir for suitable definitions.
# Set to TRUE if you want to only use the inputs provided

#' (Deprecated) Print a matrix, allowing fractions or LaTeX output
#'
#' @param A       A numeric matrix
#' @param parent  flag used to search in the parent envir for suitable definitions of other arguments.
#'                Set to \code{TRUE} (the default) if you want to only use the inputs provided.
#' @param fractions If \code{TRUE}, print numbers as rational fractions, using the \code{\link[MASS]{fractions}}
#'    function; if you require greater accuracy, you can set the \code{cycles} (default 10)
#'    and/or \code{max.denominator} (default 2000) arguments to \code{fractions} as a global option, e.g.,
#'    \code{options(fractions=list(cycles=100, max.denominator=10^4))}.
#' @param latex   If \code{TRUE}, print the matrix in LaTeX format
#' @param tol     Tolerance for rounding small numbers to 0
#'
#' @return        The formatted matrix
#' @seealso \code{\link[MASS]{fractions}}
#' @export
#'
#' @examples
#' A <- matrix(1:12, 3, 4) / 6
#' printMatrix(A, fractions=TRUE)
#' printMatrix(A, latex=TRUE)

printMatrix <- function(A, parent = TRUE, fractions = FALSE, latex = FALSE,
						tol = sqrt(.Machine$double.eps)){
    .Deprecated("printMatrix",
                msg="Function is deprecated. See latexMatrix() and Eqn() for more recent approaches")
	if(parent){
		envir <- as.environment(-1L)
		if(!is.null(envir$fractions)) fractions <- envir$fractions
		if(!is.null(envir$latex)) latex <- envir$latex
		if(!is.null(envir$tol)) tol <- envir$tol
	}
	if (latex) {
		matrix2latex(A, fractions=fractions, digits = round(abs(log(tol,10))))
	} else {
		print(formatNumbers(as.matrix(A), fractions=fractions, tol=tol))
	}
}

formatNumbers <- function(x, fractions=FALSE, tol=sqrt(.Machine$double.eps)){
	ret <- if (fractions) Fractions(x) else round(x, round(abs(log(tol, 10))))
	ret
}

# radians and degrees
r2d <-function(r) r/pi * 180

d2r <-function(d) pi*d / 180

rad2deg <- function(rad) (rad * 180) / pi

deg2rad <- function(deg) (deg * pi) / 180

Fractions <- function(x, ...){
  opts <- getOption("fractions")
  if (is.null(opts)){
    cycles <- 10
    max.denominator <- 2000
  } else {
    cycles <- opts$cycles
    max.denominator <- opts$max.denominator
    if (is.null(cycles)) cycles <- 10
    if (is.null(max.denominator)) max.denominator <- 2000
  }
  MASS::fractions(x, cycles=cycles, max.denominator=max.denominator, ...)
}

