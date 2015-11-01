################################
# -- Elementary row operations
################################

#' Add multiples of rows to other rows
#'
#' The elementary row operation \code{rowadd} adds multiples of one or more rows to other rows of a matrix.
#' This is usually used as a means to solve systems of linear equations, of the form \eqn{A x = b}, and \code{rowadd}
#' corresponds to adding equals to equals.
#'
#' The functions \code{\link{rowmult}} and \code{\link{rowswap}} complete the basic operations used in reduction
#' to row echelon form and Gaussian elimination. These functions are used for demonstration purposes.

#' @param x a numeric matrix, often consisting of the coeficient matrix, A, joined with a vector of constants, b.
#' @param from the index of one or more source rows. If \code{from} is a vector, it must have the same length as \code{to}.
#' @param to the index of one or more destination rows
#' @param mult the multiplier(s)
#' @return the matrix \code{x}, as modified
#' @seealso \code{\link{echelon}}, \code{\link{gaussianElimination}}
#' @family elementary row operations
#' @examples
#' A <- matrix(c(2, 1, -1,
#'              -3, -1, 2,
#'              -2,  1, 2), 3, 3, byrow=TRUE)
#' b <- c(8, -11, -3)
#'
#' # using row operations to reduce below diagonal to 0
#' Ab <- cbind(A, b)
#' (Ab <- rowadd(Ab, 1, 2, 3/2))  # row 2 <- row 2 + 3/2 row 1
#' (Ab <- rowadd(Ab, 1, 3, 1))    # row 3 <- row 3 + 1 row 1
#' (Ab <- rowadd(Ab, 2, 3, -4))   # row 3 <- row 3 - 4 row 2
#' # multiply to make diagonals = 1
#' (Ab <- rowmult(Ab, 1:3, c(1/2, 2, -1)))
#' # The matrix is now in triangular form
#'
#' # Could continue to reduce above diagonal to zero
#' echelon(A, b, verbose=TRUE, fractions=TRUE)
#'

rowadd <- function(x, from, to, mult) {
  if (!is.numeric(x) || !is.matrix(x)) stop("x must be a numeric matrix")
  y <- x
  y[to,] <- y[to,] + mult * y[from,]
  y
}

#' Interchange two rows of a matrix

#' This elementary row operation corresponds to interchanging two equations.
#'
#' @param x a matrix, often consisting of the coeficient matrix, A, joined with a vector of constants, b.
#' @param from source row.
#' @param to destination row
#' @return the matrix \code{x}, with rows \code{from} and \code{to} interchanged
#' @seealso \code{\link{echelon}}, \code{\link{gaussianElimination}}
#' @family elementary row operations
#'
rowswap <- function(x, from, to) {
  if (!is.numeric(x) || !is.matrix(x)) stop("x must be a numeric matrix")
  y <- x
  y[c(to,from),] <- y[c(from,to),]
  y
}

#' Multiply Rows by Constants
#'
#' Multiplies one or more rows of a matrix by constants. This corresponds to multiplying or dividing equations
#' by constants.
#'
#' @param x a matrix, often consisting of the coeficient matrix, A, joined with a vector of constants, b.
#' @param row index of one or more rows.
#' @param mult row multiplier(s)
#' @return the matrix \code{x}, with rows \code{from} and \code{to} interchanged
#' @seealso \code{\link{echelon}}, \code{\link{gaussianElimination}}
#' @family elementary row operations
#' @examples
#' A <- matrix(c(2, 1, -1,
#'              -3, -1, 2,
#'              -2,  1, 2), 3, 3, byrow=TRUE)
#' b <- c(8, -11, -3)
#'
#' # using row operations to reduce below diagonal to 0
#' Ab <- cbind(A, b)
#' (Ab <- rowadd(Ab, 1, 2, 3/2))  # row 2 <- row 2 + 3/2 row 1
#' (Ab <- rowadd(Ab, 1, 3, 1))    # row 3 <- row 3 + 1 row 1
#' (Ab <- rowadd(Ab, 2, 3, -4))
#' # multiply to make diagonals = 1
#' (Ab <- rowmult(Ab, 1:3, c(1/2, 2, -1)))
#' # The matrix is now in triangular form
#'

rowmult <- function(x, row, mult) {
  if (!is.numeric(x) || !is.matrix(x)) stop("x must be a numeric matrix")
  y <- x
  y[row,] <- mult * y[row,]
  y
}

