#' Test for square matrix
#'
#' @param x A numeric matrix
#'
#' @return \code{TRUE} if \code{x} is a square matrix, else \code{FALSE}
#' @keywords internal
#'

is_square_matrix <- function (x)
{
  return(is.matrix(x) && nrow(x) == ncol(x))
}

#' Test for square, symmetrix matrix
#'
#' @param x A numeric matrix
#'
#' @return \code{TRUE} if \code{x} is a square, symmetrix matrix, else \code{FALSE}
#' @keywords internal
#'
is_symmetric_matrix <- function (x)
{
  tol <- 10*sqrt(.Machine$double.eps)
  return (is_square_matrix(x) && all(abs(x - t(x)) < tol))
}

