#' QR Decomposition by Graham-Schmidt Orthonormalization
#'
#' \code{QR} computes the QR decomposition of a matrix, \eqn{X}, that is an orthonormal matrix, \eqn{Q} and an upper triangular
#' matrix, \eqn{R}, such that \eqn{X = Q R}.
#'
#' The QR decomposition plays an important role in many statistical techniques.
#' In particular it can be used to solve the equation \eqn{Ax = b} for given matrix \eqn{A} and vector \eqn{b}.
#' The function is included here simply to show the algorithm of Gram-Schmidt orthogonalization.  The standard
#' \code{\link[base]{qr}} function is faster and more accurate.
#'
#' @param X a numeric matrix
#' @param tol tolerance for detecting linear dependencies in the columns of \code{X}
#' @return a list of three elements, consisting of an orthonormal matrix \code{Q}, an upper triangular matrix \code{R}, and the \code{rank}
#'     of the matrix \code{X}
#' @author John Fox and Georges Monette
#' @seealso \code{\link[base]{qr}}
#' @export
#' @examples
#' A <- matrix(c(1,2,3,4,5,6,7,8,10), 3, 3) # a square nonsingular matrix
#' res <- QR(A)
#' res
#' q <- res$Q
#' zapsmall( t(q) %*% q)   # check that q' q = I
#' r <- res$R
#' q %*% r                 # check that q r = A
#'
#' # relation to determinant: det(A) = prod(diag(R))
#' det(A)
#' prod(diag(r))
#'
#' B <- matrix(1:9, 3, 3) # a singular matrix
#' QR(B)

QR <- function(X, tol=sqrt(.Machine$double.eps)){
  # QR decomposition by Graham-Schmidt orthonormalization
  # X: a matrix
  # tol: 0 tolerance
  if (!is.numeric(X) || !is.matrix(X)) stop("X must be a numeric matrix")
  length <- function(u) sqrt(sum(u^2))
  U <- X
  E <- matrix(0, nrow(X), ncol(X))
  E[, 1] <- U[, 1]/length(U[, 1])
  for (j in 2:ncol(U)){
    for (k in 1:(j - 1)){
      U[, j] <- U[, j] - (X[, j] %*% E[, k]) * E[, k]
    }
    len.U.j <- length(U[, j])
    if (len.U.j > tol) E[, j] <- U[, j]/len.U.j
  }
  R <- t(E) %*% X
  R[abs(R) < tol] <- 0
  rank <- sum(rowSums(abs(R)) > 0)
  list(Q=-E, R=-R, rank=rank) # negated to match qr()
}
