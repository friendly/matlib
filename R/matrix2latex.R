#' Convert matrix to LaTeX equation
#'
#' This function provides a soft-wrapper to \code{xtable::xtableMatharray()} with support for
#' \code{fraction}s output and square \code{bracket}s.
#'
#' @param x a matrix
#' @param fractions logical; if \code{TRUE}, try to express non-integers as rational numbers, using the \code{\link[MASS]{fractions}} 
#'    function; if you require greater accuracy, you can set the \code{cycles} (default 10)
#'    and/or \code{max.denominator} (default 2000) arguments to \code{fractions} as a global option, e.g.,
#'    \code{options(fractions=list(cycles=100, max.denominator=10^4))}.
#' @param brackets logical; include square brackets around the matrices?
#' @param ... additional arguments passed to \code{xtable::xtableMatharray()}
#' @importFrom xtable xtableMatharray
#' @author Phil Chalmers
#' @export
#' @examples
#' A <- matrix(c(2, 1, -1,
#'              -3, -1, 2,
#'              -2,  1, 2), 3, 3, byrow=TRUE)
#' b <- c(8, -11, -3)
#'
#' matrix2latex(cbind(A,b))
#' matrix2latex(cbind(A,b), digits = 0)
#' matrix2latex(cbind(A/2,b), fractions = TRUE)
#'
matrix2latex <- function(x, fractions = FALSE, brackets = TRUE, ...){
  ret <- if (fractions) xtable::xtableMatharray(as.character(Fractions(x)), ...)
    else xtable::xtableMatharray(x, ...)
  if(brackets) cat('\\left[\n ')
  print(ret)
  if(brackets) cat('\\right]\n')
  invisible(NULL)
}
