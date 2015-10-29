#' Eigen Decomposition of a Square Symmetrix Matrix
#'
#' \code{eig} calculates the eigenvalues and eigenvectors of a square, symmetric matrix using the iterated QR decomposition
#'
#' @param X a square symmetrix matrix
#' @param tol tolerance passed to \code{\link{QR}}
#' @param max.iter maximum number of QR iterations
#' @param retain.zeroes logical; retain 0 eigenvalues?
#' @return a list of two elements: \code{values}-- eigenvalues, \code{vectors}-- eigenvectors
#' @author John Fox and Georges Monette
#' @seealso \code{\link[base]{eigen}}
#' @seealso \code{\link{SVD}}
#' @examples
#' C <- matrix(c(1,2,3,2,5,6,3,6,10), 3, 3) # nonsingular, symmetric
#' C
#' EC <- eig(C) # eigenanalysis of C
#' EC$vectors %*% diag(EC$values) %*% t(EC$vectors) # check

eig <- function(X, tol=sqrt(.Machine$double.eps), max.iter=100, retain.zeroes=TRUE){
  # returns the eigenvalues and eigenvectors of a square, symmetric matrix using the iterated QR decomposition
  # X: a square, symmetric matrix
  # tol: 0 tolerance
  # max.iter: iteration limit
  # retain.zeroes: retain 0 eigenvalues?
  if (!is.numeric(X) || !is.matrix(X) || nrow(X) != ncol(X) || any(abs(X - t(X)) > tol))
    stop("X must be a numeric, square, symmetric matrix")
  i <- 1
  Q <- diag(nrow(X))
  while (i <= max.iter){
    qr <- QR(X, tol=tol)
    Q <- Q %*% qr$Q
    X <- qr$R %*% qr$Q
    if (max(abs(X[lower.tri(X)])) <= tol) break
    i <- i + 1
  }
  if (i > max.iter) warning("eigenvalues did not converge")
  values <- diag(X)
  if (!retain.zeroes){
    nonzero <- values != 0
    values <- values[nonzero]
    Q <- Q[, nonzero, drop=FALSE]
  }
  list(values=values, vectors=Q)
}

#' Singular Value Decomposition of a Matrix
#'
#' Compute the singular-value decomposition of a matrix X from the eigenstructure of X'X
#'
#' @param X a square symmetrix matrix
#' @param tol tolerance passed to \code{\link{QR}}
#' @return a list of three elements: \code{d}-- singular values, \code{U}-- left singular vectors, \code{V}-- right singular vectors
#' @author John Fox and Georges Monette
#' @seealso \code{\link[base]{eigen}}
#' @seealso \code{\link{SVD}}
#' @examples
#' C <- matrix(c(1,2,3,2,5,6,3,6,10), 3, 3) # nonsingular, symmetric
#' C
#' SVD(C)

SVD <- function(X, tol=sqrt(.Machine$double.eps)){
  # compute the singular-value decomposition of a matrix X from the eigenstructure of X'X
  # X: a matrix
  # tol: 0 tolerance
  VV <- eig(t(X) %*% X, tol=tol, retain.zeroes=FALSE)
  V <- VV$vectors
  d <- sqrt(VV$values)
  U <- X %*% V %*% diag(1/d,nrow=length(d)) # magically orthogonal
  list(d=d, U=U, V=V)
}
