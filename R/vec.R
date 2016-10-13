#' Vectorize a Matrix
#'
#' Returns a 1-column matrix, stacking the columns of \code{x}, a matrix or vector
#'
#' @param x A matrix or vector
#'
#' @return A one-column matrix containing the elements of \code{x} in column order
#'
#' @export
#' @examples
#' vec(1:3)
#' vec(matrix(1:6, 2, 3))
#' vec(c("hello", "world"))
vec <- function (x)
{
  if (!(is.matrix(x) || is.vector(x))) {
    stop("argument x is not a matrix or vector")
  }
#   if (!is.numeric(x)) {
#     stop("argument x is not numeric")
#  }
  return(t(t(as.vector(x))))
}
