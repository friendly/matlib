#' Test for square matrix
#'
#' @param x A numeric matrix
#'
#' @return \code{TRUE} if \code{x} is a square matrix, else \code{FALSE}
#' @keywords internal
#' @rdname is_square
#'

is_square_matrix <- function (x)
{
  return(is.matrix(x) && nrow(x) == ncol(x))
}

#' Test for square, symmetric matrix
#'
#' @param x A numeric matrix
#'
#' @return \code{TRUE} if \code{x} is a square, symmetric matrix, else \code{FALSE}
#' @keywords internal
#' @rdname is_square
#'
is_symmetric_matrix <- function (x)
{
  tol <- 10*sqrt(.Machine$double.eps)
  return (is_square_matrix(x) && all(abs(x - t(x)) < tol))
}

#' Test for orthogonal matrix
#'
#' @param x A numeric matrix
#'
#' @return \code{TRUE} if \code{x} is an orthogonal matrix, else \code{FALSE}
#'
#' @rdname is_square
is_orthogonal_matrix <- function (x)
{
  tol <- 10*sqrt(.Machine$double.eps)
  xx <- crossprod(x)
  diag(xx) <- 0
  return(all(xx < tol))
}

