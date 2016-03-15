#' Convert matrix to LaTeX equation
#'
#' This function provides a soft-wrapper to \code{xtable::xtableMatharray()} with support for
#' \code{fraction}s output and square \code{bracket}s.
#'
#' @param x a matrix
#' @param fractions logical; if \code{TRUE}, try to express non-integers as rational numbers
#' @param brackets logical; include square brackets around the matricies?
#' @param ... additional arguments passed to \code{xtable::xtableMatharray()}
#' @author Phil Chalmers
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
  if (fractions) {
    mass <- requireNamespace("MASS", quietly=TRUE)
    if (!mass) stop("fractions=TRUE needs MASS package")
  }
  xtable <- requireNamespace("xtable", quietly=TRUE)
  if (!xtable) stop("latex=TRUE needs xtable package")
  ret <- if (fractions) xtable::xtableMatharray(as.character(MASS::fractions(x)), ...)
    else xtable::xtableMatharray(x, ...)
  if(brackets) cat('\\left[\n ')
  print(ret)
  if(brackets) cat('\\right]\n')
  invisible(NULL)
}