#' Create a vector, matrix or array of constants
#'
#' This function creates a vector, matrix or array of constants, typically used for
#' the unit vector or unit matrix in matrix expressions.
#'
#' The "dimnames" attribute is optional: if present it is a list with one component for each dimension,
#' either NULL or a character vector of the length given by the element of the "dim" attribute for that
#' dimension. The list can be named, and the list names will be used as names for the dimensions.
#'
#' @param ...      One or more arguments supplying the dimensions of the array, all non-negative integers
#' @param constant The value of the constant used in the array
#' @param dimnames Either \code{NULL} or the names for the dimensions.
#' @examples
#' J(3)
#' J(2,3)
#' J(2,3,2)
#' J(2,3, constant=2, dimnames=list(letters[1:2], LETTERS[1:3]))
#'
#' X <- matrix(1:6, nrow=2, ncol=3)
#' dimnames(X) <- list(sex=c("M", "F"), day=c("Mon", "Wed", "Fri"))
#' J(2) %*% X      # column sums
#' X %*% J(3)      # row sums

J <- function(..., constant=1, dimnames=NULL) {
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  args <- list(...)
  if (!all(sapply(args, is.wholenumber)))
    stop("Supply only integers for the dimensions of the array")
  if (any(unlist(args) < 0))
    stop("Supply only non-negative dimension sizes")

  if (length(args) == 1) {
    res <- rep(constant, args[[1]])
    # TODO: handle dimnames
  }
  else # if (length(args) == 2)
    res <- array(constant, dim=unlist(args), dimnames=dimnames)

  res
}
