#' LU Decomposition
#'
#' \code{LU} computes the LU decomposition of a matrix, \eqn{A}, such that \eqn{P A = L U},
#' where \eqn{L} is a lower triangle matrix, \eqn{U} is an upper triangle, and \eqn{P} is a
#' permutation matrix.
#'
#' The LU decomposition is used to solve the equation \eqn{A x = b} by calculating
#' \eqn{L(Ux - d) = 0}, where \eqn{Ld = b}. If row exchanges are necessary for
#' \eqn{A} then the permutation matrix \eqn{P} will be requred to exchange the rows in \eqn{A};
#' otherwise, \eqn{P} will be an identity matrix and the LU equation will be simplified to
#' \eqn{A = L U}.
#'
#' @param A coefficient matrix
#' @param b right-hand side vector. When supplied the returned object will also contain the solved
#'   \eqn{d} and \code{x} elements
#' @param tol tolerance for checking for 0 pivot
#' @param fractions logical; if \code{TRUE}, try to express non-integers as rational numbers
#' @author Phil Chalmers
#' @export
#' @examples
#'
#'   A <- matrix(c(2, 1, -1,
#'                -3, -1, 2,
#'                -2,  1, 2), 3, 3, byrow=TRUE)
#'   b <- c(8, -11, -3)
#'   (ret <- LU(A)) # P is an identity; no row swapping
#'   with(ret, L %*% U) # check that A = L * U
#'   LU(A, b)
#'
#'   # permutations required in this example
#'   A <- matrix(c(1,  1, -1,
#'                 2,  2,  4,
#'                 1, -1,  1), 3, 3, byrow=TRUE)
#'   b <- c(1, 2, 9)
#'   (ret <- LU(A, b))
#'   with(ret, P %*% A)
#'   with(ret, L %*% U)
#'
LU <- function(A, b, tol=sqrt(.Machine$double.eps), fractions=FALSE){
  fbsolve <- function(mat, y){
    backword <- which(rowSums(mat == 0) == max(rowSums(mat == 0))) > 1L
    len <- length(y)
    seq <- if(!backword) 1L:len else len:1L
    ret <- numeric(len)
    for(i in seq){
      if(i == seq[1L]){
        ret[i] <- y[i] / mat[i,i]
      } else {
        ret[i] <- (y[i]- sum(mat[i, -i] * ret[-i]) ) / mat[i,i]
      }
    }
    ret
  }
  if (fractions) {
    mass <- requireNamespace("MASS", quietly=TRUE)
    if (!mass) stop("fractions=TRUE needs MASS package")
  }
  if ((!is.matrix(A)) || (!is.numeric(A)))
    stop("argument must be a numeric matrix")
  n <- nrow(A)
  m <- ncol(A)
  wasmissing <- missing(b)
  if(wasmissing) b <- matrix(0, n)
  else b <- as.matrix(b)
  i <- j <- 1
  L <- P <- diag(n)
  while (i <= n && j <= m){
    while (j <= m){
      for (k in 1:m){
        if (k <= j) next
        if(i == j && i < n && abs(A[i,i]) <= tol){ #check if 0 pivot
          A <- rowswap(A, i, i + 1L)
          P <- rowswap(P, i, i + 1L)
          diag(L) <- 0
          L <- rowswap(L, i, i + 1L)
          diag(L) <- 1
          L[upper.tri(L)] <- 0
          b <- rowswap(b, i, i + 1L)
          next
        }
        L[k,i] <- A[k, j]/A[i, j]
        A <- rowadd(A, i, k, -A[k, j]/A[i, j])
      }
      j <- j + 1
      break
    }
    i <- i + 1
  }
  # 0 rows to bottom
  zeros <- which(apply(A[,1:m], 1, function(x) max(abs(x)) <= tol))
  if (length(zeros) > 0){
    zeroRows <- A[zeros,]
    A <- A[-zeros,]
    A <- rbind(A, zeroRows)
  }
  rownames(A) <- NULL
  ret <- list(P=P, L=L, U=A)
  if(!wasmissing){
    ret$d <- fbsolve(L, b)
    ret$x <- fbsolve(A, ret$d)
  }
  ret <- if (fractions) lapply(ret, MASS::fractions)
  else lapply(ret, function(x) round(x, round(abs(log(tol, 10)))))
  ret <- lapply(ret, function(x){attr(x, 'T') <- NULL; x})
  ret
}
