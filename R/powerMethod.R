
#' @title Power Method for Eigenvectors
#'
#' @description Finds a dominant eigenvalue, \eqn{\lambda_1}, and its corresponding
#' eigenvector, \eqn{v_1}, of a square matrix by applying Hotelling's (1933) Power Method with scaling.
#'
#' @details
#' The method is based upon the fact that repeated multiplication of a matrix \eqn{A} by a trial
#' vector \eqn{v_1^{(k)}} converges to the value of the eigenvector,
#' \deqn{v_1^{(k+1)} = A v_1^{(k)} / \vert\vert A v_1^{(k)} \vert\vert }
#' The corresponding eigenvalue is then found as
#' \deqn{\lambda_1 = \frac{v_1^T A v_1}{v_1^T  v_1}}
#'
#' In pre-computer days, this method could be extended to find subsequent eigenvalue - eigenvector
#' pairs by "deflation", i.e., by applying the method again to the new matrix.
#' \eqn{A - \lambda_1 v_1 v_1^{T} }.
#'
#' This method is still used in some computer-intensive applications with huge matrices where only the
#' dominant eigenvector is required, e.g., the Google Page Rank algorithm.
#'
#'
#' @param A a square numeric matrix
#' @param v optional starting vector; if not supplied, it uses a unit vector of length equal to the number of rows / columns of \code{x}.
#' @param eps convergence threshold for terminating iterations
#' @param maxiter maximum number of iterations
#' @param plot logical; if \code{TRUE}, plot the series of iterated eigenvectors?
#' @return a list containing the eigenvector (\code{vector}), eigenvalue (\code{value}), iterations (\code{iter}),
#'   and iteration history (\code{vector_iterations})
#' @importFrom grDevices adjustcolor
#' @export
#' @references Hotelling, H. (1933). Analysis of a complex of statistical variables into principal components. \emph{Journal of Educational Psychology}, 24, 417-441, and 498-520.
#' @author Gaston Sanchez (from matrixkit)
#' @examples
#' A <- cbind(c(7, 3), c(3, 6))
#' powerMethod(A)
#' eigen(A)$values[1] # check
#' eigen(A)$vectors[,1]
#'
#' # demonstrate how the power method converges to a solution
#' powerMethod(A, v = c(-.5, 1), plot = TRUE)
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

powerMethod <- function(A, v = NULL, eps = 1e-6, maxiter = 100, plot=FALSE)
{
  if (!is_square_matrix(A))
    stop("'powerMethod()' requires a square numeric matrix")

  if (!is.null(v))
  {
    if (!is.vector(v) || !is.numeric(v))
      stop("'powerMethod()' requires 'v' to be a numeric vector")
    if (nrow(A) != length(v))
      stop("'A' is not conformable with 'v' in 'powerMethod()'")
  } else {
    v = rep(1, nrow(A))
  }

  if (!eps > 0)
    eps = 1e-6

  v_old = v
  steps = 1
  vectors <- list(v)
  repeat
  {
    v_new = A %*% v_old
    v_new = v_new / len(v_new)
    if (len(abs(v_new) - abs(v_old)) <= eps) break
    v_old = v_new
    steps = steps + 1
    vectors[[steps]] <- c(v_new)
    if (steps == maxiter) break
  }
  # Rayleigh quotient gives the eigenvalue
  lambda = sum((A %*% v_new) * v_new)
  # output
  res <- list(iter = steps, vector = v_new, value = lambda)
  vectors <- do.call(cbind, vectors)
  colnames(vectors) <- paste0("v", 1:ncol(vectors))
  res <- c(vector_iterations=list(vectors), res)
  if(plot){
      vecs <- t(vectors)
      pos <- c(min(c(vecs, 0))-.1, max(vecs) + .1)
      plot(pos, pos, type="n", xlab="x1", ylab="x2")
      col <- sapply(1:nrow(vecs) / nrow(vecs), 
      			  function(x) grDevices::adjustcolor('red', x))
      vectors(vecs, col=col)
      abline(h=0, v=0, col="gray")
      return(invisible(res))
  }
  res
}
