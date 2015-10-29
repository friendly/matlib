################################################
# Some useful R functions for matrix operations
################################################

# These functions are mainly for tutorial purposes in learning matrix algebra
# ideas using R. In some cases, functions are provided for concepts available
# elsewhere in R, but where the function call or name is not obvious.  In other
# cases, functions are provided to show or demonstrate an algorithm.

# These are not meant for production uses. Little errror checking is done.


#' Rank of a Matrix
#'
#' Returns the rank of a matrix \code{x}, using the QR decomposition, \code{qr()}.
#' Included here as a simple function, because \code{rank} does something different
#' and it is not obvious what to use for matrix rank.
#'
#' @param x a matrix
#' @return rank of \code{x}
#'
R <- function(x)  qr(x)$rank

#' Projection of Vector y on columns of X
#'
#' @param y a vector, treated as a one-column matrix
#' @param X a vector or matrix.  Number of rows of \code{y} and \code{X} must match
#' @return the projection of y on X
#' @examples
#' X <- matrix( c(1, 1, 1, 1, 1, -1, 1, -1), 4,2, byrow=TRUE)
#' y <- 1:4
#' proj(y, X[,1])  # project y on unit vector
#' proj(y, X[,2])
#' proj(y, X)

proj <- function(y, X) {
  if (is.vector(y)) y <- matrix(y, ncol=1)
  if (is.vector(X)) X <- matrix(X, ncol=1)
   XPX <- crossprod(X)   # t(X) %*% X
   P <- X %*% MASS::ginv(XPX) %*% t(X)
#   P <- X %*% Ginv(XPX) %*% t(X)
   P %*% y
}









