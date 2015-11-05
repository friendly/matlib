#' Projection of Vector y on columns of X
#'
#' Fitting a linear model, \code{lm(y ~ X)}, by least squares can be thought of geometrically as the projection of
#' \code{y} on the column space of \code{X}.  This function is designed to allow exploration of projections
#' and orthogonality.
#'
#' @param y a vector, treated as a one-column matrix
#' @param X a vector or matrix.  Number of rows of \code{y} and \code{X} must match
#' @return the projection of \code{y} on \code{X}
#' @author Michael Friendly
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
