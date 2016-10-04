
#' @title Power Method for Eigenvectors
#'
#' @description Finds a dominant eigenvalue and its corresponding
#' eigenvector of a square matrix by applying the Power Method with scaling
#'
#' @param X a square numeric matrix
#' @param v optional starting vector
#' @param eps convergence threshold
#' @param maxiter maximum number of iterations
#' @param verbose logical; if \code{TRUE}, show the approximation to the eigenvector at each iteration
#' @return a list containing the eigenvector, eigenvalue, and iterations
#' @export
#' @author Gaston Sanchez (from matrixkit)
#' @examples
#' A = cbind(c(2, 1), c(12, 5))
#' powerMethod(A)
#' eigen(A)$vectors[,1]  # check
#'
#' B = cbind(c(1, 2, 0), c(2, 1, 3), c(0, 3, 1))
#' powerMethod(B)

powerMethod <- function(X, v = NULL, eps = 1e-6, maxiter = 100, verbose=FALSE)
{
  if (!is_square_matrix(X))
    stop("'powerMethod()' requires a square numeric matrix")

  if (!is.null(v))
  {
    if (!is.vector(v) || !is.numeric(v))
      stop("'powerMethod()' requires 'v' to be a numeric vector")
    if (nrow(X) != length(v))
      stop("'X' is not conformable with 'v' in 'powerMethod()'")
  } else {
    v = rep(1, nrow(X))
  }

  if (!eps > 0)
    eps = 1e-6

  v_old = v
  steps = 1
  repeat
  {
    v_new = X %*% v_old
    v_new = v_new / len(v_new)
    if (verbose) cat("iter", steps, ": vector=", c(v_new), "\n"  )
    if (len(abs(v_new) - abs(v_old)) <= eps) break
    v_old = v_new
    steps = steps + 1
    if (steps == maxiter) break
  }
  # Rayleigh quotient gives the eigenvalue
  lambda = sum((X %*% v_new) * v_new)
  # output
  list(vector = v_new, value = lambda, iter = steps)
}
