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

#' @param x a numeric matrix, possibly consisting of the coefficient matrix, A, joined with a vector of constants, b.
#' @param from the index of one or more source rows. If \code{from} is a vector, it must have the same length as \code{to}.
#' @param to the index of one or more destination rows
#' @param mult the multiplier(s)
#' @return the matrix \code{x}, as modified
#' @seealso \code{\link{echelon}}, \code{\link{gaussianElimination}}
#' @family elementary row operations
#' @export
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
  if(is.null(attr(y, 'T'))) attr(y, 'T') <- list()
  T <- diag(nrow(x))
  T[to, from] <- mult
  attr(y, 'T')[[length(attr(y, 'T')) + 1L]] <- T
  class(y) <- c('trace', 'matrix')
  y
}

#' Interchange two rows of a matrix
#'
#' This elementary row operation corresponds to interchanging two equations.
#'
#' @param x a matrix, possibly consisting of the coefficient matrix, A, joined with a vector of constants, b.
#' @param from source row.
#' @param to destination row
#' @return the matrix \code{x}, with rows \code{from} and \code{to} interchanged
#' @seealso \code{\link{echelon}}, \code{\link{gaussianElimination}}
#' @export
#' @family elementary row operations
#'
rowswap <- function(x, from, to) {
  if (!is.numeric(x) || !is.matrix(x)) stop("x must be a numeric matrix")
  y <- x
  y[c(to,from),] <- y[c(from,to),]
  if(is.null(attr(y, 'T'))) attr(y, 'T') <- list()
  T <- diag(nrow(x))
  T[from, from] <- T[to, to] <- 0
  T[from, to] <- T[to, from] <- 1
  attr(y, 'T')[[length(attr(y, 'T')) + 1L]] <- T
  class(y) <- c('trace', 'matrix')
  y
}

#' Multiply Rows by Constants
#'
#' Multiplies one or more rows of a matrix by constants. This corresponds to multiplying or dividing equations
#' by constants.
#'
#' @param x a matrix, possibly consisting of the coefficient matrix, A, joined with a vector of constants, b.
#' @param row index of one or more rows.
#' @param mult row multiplier(s)
#' @return the matrix \code{x}, modified
#' @seealso \code{\link{echelon}}, \code{\link{gaussianElimination}}
#' @family elementary row operations
#' @export
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
  if(is.null(attr(y, 'T'))) attr(y, 'T') <- list()
  T <- diag(nrow(x))
  T[row, row] <- mult
  attr(y, 'T')[[length(attr(y, 'T')) + 1L]] <- T
  class(y) <- c('trace', 'matrix')
  y
}

#' Build/Get transformation matrices
#'
#' Recover the history of the row operations that have been performed.
#' This function combines the transformation matrices into a single transformation matrix
#' representing all row operations or may optionally print all the individual operations which have
#' been performed.
#'
#' @param x a matrix A, joined with a vector of constants, b, that has been passed to
#'   \code{\link{gaussianElimination}} or the row operator matrix functions
#' @param all logical; print individual transformation ies?
#' @param ... additional arguments
#' @return the transformation matrix or a list of individual transformation matrices
#' @seealso \code{\link{echelon}}, \code{\link{gaussianElimination}}
#' @family matrix of elementary row operations
#' @author Phil Chalmers
#' @export
#' @examples
#' A <- matrix(c(2, 1, -1,
#'              -3, -1, 2,
#'              -2,  1, 2), 3, 3, byrow=TRUE)
#' b <- c(8, -11, -3)
#'
#' # using row operations to reduce below diagonal to 0
#' Abt <- Ab <- cbind(A, b)
#' Abt <- rowadd(Abt, 1, 2, 3/2)
#' Abt <- rowadd(Abt, 1, 3, 1)
#' Abt <- rowadd(Abt, 2, 3, -4)
#' Abt
#'
#' # build T matrix and multiply by original form
#' (T <- buildTmat(Abt))
#' T %*% Ab    # same as Abt
#'
#' # print all transformation matrices
#' buildTmat(Abt, TRUE)
#'
#' # invert transformation matrix to reverse operations
#' inv(T) %*% Abt
#'
#' # gaussian elimination
#' (soln <- gaussianElimination(A, b))
#' T <- buildTmat(soln)
#' inv(T) %*% soln
#'
buildTmat <- function(x, all = FALSE){
  if(is.null(attr(x, 'T'))) stop('No trace objects were found')
  Tlist <- attr(x, 'T')
  if(all){
    names(Tlist) <- paste0('T', 1:length(Tlist))
    return(Tlist)
  }
  T <- Tlist[[1L]]
  for(i in 2L:length(Tlist))
    T <- Tlist[[i]] %*% T
  T
}

#' @rdname buildTmat
#' @export
as.matrix.trace <- function(x, ...){
  class(x) <- 'matrix'
  attr(x, 'T') <- NULL
  x
}

#' @rdname buildTmat
#' @export
print.trace <- function(x, ...){
  print(as.matrix(x))
}
