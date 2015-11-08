#' Projection of Vector y on columns of X
#'
#' Fitting a linear model, \code{lm(y ~ X)}, by least squares can be thought of geometrically as the orthogonal projection of
#' \code{y} on the column space of \code{X}. This function is designed to allow exploration of projections
#' and orthogonality.
#'
#' The projection is defined as \eqn{P y} where \eqn{P = x (x'X)^- X'}
#' and \eqn{X^-} is a generalized inverse.
#'
#' @param y a vector, treated as a one-column matrix
#' @param X a vector or matrix.  Number of rows of \code{y} and \code{X} must match
#' @param list logical; if FALSE, return just the projected vector; otherwise returns a list
#' @return the projection of \code{y} on \code{X} (if \code{list=FALSE}) or a list with elements \code{y} and \code{P}
#' @author Michael Friendly
#' @examples
#' X <- matrix( c(1, 1, 1, 1, 1, -1, 1, -1), 4,2, byrow=TRUE)
#' y <- 1:4
#' Proj(y, X[,1])  # project y on unit vector
#' Proj(y, X[,2])
#' Proj(y, X)

Proj <- function(y, X, list=FALSE) {
  if (is.vector(y)) y <- matrix(y, ncol=1)
  if (is.vector(X)) X <- matrix(X, ncol=1)
  XPX <- crossprod(X)   # t(X) %*% X
  P <- X %*% MASS::ginv(XPX) %*% t(X)
  #   P <- X %*% Ginv(XPX) %*% t(X)
  if (!list) return(P %*% y)
  else return(list(y=P %*% y, P=P))
}
