#' Vandermode Matrix
#'
#' The function returns the Vandermode matrix of a numeric vector, \code{x}, whose columns are the vector
#' raised to the powers \code{0:n}.
#'
#' @param x a numeric vector
#' @param n a numeric scalar
#'
#' @return a matrix of size \code{length(x)} x \code{n}
#'
#' @examples
#' vandermode(1:5, 4)
vandermode <- function (x, n) {
  if (!is.vector(x))
    stop("argument x is not a vector")
  if (!is.numeric(n))
    stop("argument n is not a numeric scalar")

  outer(x, 0:(n-1), "^")
}
