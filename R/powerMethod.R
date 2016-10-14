
#' @title Power Method for Eigenvectors
#'
#' @description Finds a dominant eigenvalue, \eqn{\lambda_1}, and its corresponding
#' eigenvector, \eqn{v_1}, of a square matrix by applying Hotelling's (1933) Power Method with scaling.
#' In pre-computer days, this method could be extended to find subsequent eigenvalue - eigenvector
#' pairs by "deflation", i.e., by applying the method again to the new matrix.
#' \eqn{A - \lambda_1 v_1 v_1^{T} }
#'
#' @param X a square numeric matrix
#' @param v optional starting vector; if not supplied, it uses a unit vector of length equal to the number of rows / columns of \code{x}.
#' @param eps convergence threshold for terminating iterations
#' @param maxiter maximum number of iterations
#' @param verbose logical; if \code{TRUE}, show the approximation to the eigenvector at each iteration
#' @param keep   logical; if \code{TRUE}, also return the series of iterated eigenvectors as another component, named \code{vectors}.
#' @return a list containing the eigenvector (\code{vector}), eigenvalue (\code{value}), and iterations (\code{iter})
#' @export
#' @references Hotelling, H. (1933). Analysis of a complex of statistical variables into principal components. \emph{Journal of Educational Psychology}, 24, 417-441, and 498-520.
#' @author Gaston Sanchez (from matrixkit)
#' @examples
#' A <- cbind(c(2, 1), c(12, 5))
#' powerMethod(A, verbose=TRUE)
#' eigen(A)$vectors[,1]  # check
#'
#' B <- cbind(c(1, 2, 0), c(2, 1, 3), c(0, 3, 1))
#' (rv <- powerMethod(B))
#'
#' # deflate to find 2nd latent vector
#' l <- rv$value
#' v <- c(rv$vector)
#' B1 <- B - l * outer(v, v)
#' powerMethod(B1)
#' eigen(B)$vectors     # check
#'
#' # a positive, semi-definite matrix, with eigenvalues 12, 6, 0
#' C <- matrix(c(7, 4, 1,  4, 4, 4,  1, 4, 7), 3, 3)
#' eigen(C)$vectors
#' powerMethod(C)


powerMethod <- function(X, v = NULL, eps = 1e-6, maxiter = 100, verbose=FALSE, keep=FALSE)
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
  vectors <- list()
  repeat
  {
    v_new = X %*% v_old
    v_new = v_new / len(v_new)
    if(keep) vectors[[steps]] <- c(v_new)
    if (verbose) cat("iter", steps, ": vector=", c(v_new), "\n"  )
    if (len(abs(v_new) - abs(v_old)) <= eps) break
    v_old = v_new
    steps = steps + 1
    if (steps == maxiter) break
  }
  # Rayleigh quotient gives the eigenvalue
  lambda = sum((X %*% v_new) * v_new)
  # output
  res <- list(vector = v_new, value = lambda, iter = steps)
  if(keep) {
    vectors <- do.call(cbind, vectors)
    res <- c(res, vectors=list(vectors))
  }
  res
}
