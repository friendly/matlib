#' Vectorize a Matrix
#'
#' Returns a 1-column matrix, stacking the columns of \code{x}, a matrix or vector.
#' Also supports comma-separated inputs similar to the concatenation
#' function \code{\link{c}}.
#'
#' @param x A matrix or vector
#'
#' @param ... (optional) additional objects to be stacked
#'
#' @return A one-column matrix containing the elements of \code{x} and \code{...}
#'   in column order
#'
#' @export
#' @examples
#' vec(1:3)
#' vec(matrix(1:6, 2, 3))
#' vec(c("hello", "world"))
#' vec("hello", "world")
#' vec(1:3, "hello", "world")
vec <- function (x, ...)
{
  if (!(is.matrix(x) || is.vector(x))) {
    stop("argument x is not a matrix or vector")
  }
  dots <- list(...)
  if(length(dots))
      x <- c(x, do.call(c, dots))
#   if (!is.numeric(x)) {
#     stop("argument x is not numeric")
#  }
  return(t(t(as.vector(x))))
}
