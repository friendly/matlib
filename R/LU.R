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
#' @param verbose logical; if \code{TRUE}, print intermediate steps
#' @param ... additional arguments passed to \code{\link{showEqn}}
#' @return A list of matrix components of the solution, \code{P}, \code{L} and \code{U}. If \code{b}
#'        is supplied, the vectors \eqn{d} and \code{x} are also returned.
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
#'   LU(A, b, verbose=TRUE)
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
LU <- function(A, b, tol=sqrt(.Machine$double.eps), fractions=FALSE, verbose=FALSE, ...){
  fbsolve <- function(mat, y, verbose){
    backword <- which(rowSums(mat == 0) == max(rowSums(mat == 0))) > 1L
    len <- length(y)
    seq <- if(!backword) 1L:len else len:1L
    ret <- numeric(len)
    ncol <- ncol(mat)
    if(verbose) cat(sprintf("\n%s operations:\n", 
    						ifelse(backword, 'Back-solving', 'Forward-solving')))
    for(i in seq){
      if(i == seq[1L]){
      	if(verbose){
      		cat('\n  Equation: ')
      		showEqn(mat[i, ,drop=FALSE], y[i], simplify=TRUE)
      		cat(sprintf("  Solution: x%i = %s/%s = %s\n", i, y[i], mat[i,i], 
      					y[i] / mat[i,i]))
      	}
        ret[i] <- y[i] / mat[i,i]
      } else {
      	if(verbose){
      		cat('\n  Equation: ')
      		showEqn(mat[i, ,drop=FALSE], y[i], simplify=TRUE)
      		pick <- if(backword) (i+1L):ncol else 1L:min(i+1L, ncol)
      		cat('  Substitution: ')
      		vars <- if(backword){
      			vars <- c(paste0('x', 1:i), ret[length(ret):(i+1L)])
      			showEqn(mat[i, ,drop=FALSE], y[i], vars = vars, simplify=TRUE)
      		} else {
      			vars <- c(ret[1:(i-1L)], paste0('x', i:ncol))
      			showEqn(mat[i, ,drop=FALSE], y[i, ,drop=FALSE], vars = vars, simplify=TRUE)
      		}
      		cat(sprintf("  Solution: x%i = (%s - %s) /%s = ", i, y[i], 
      					paste0(sum(mat[i, pick] * ret[pick]), collapse=' - '), mat[i,i]))
      	} 
        ret[i] <- (y[i]- sum(mat[i, -i] * ret[-i]) ) / mat[i,i]
        if(verbose) cat(sprintf("%s\n", ret[i]))
      }
    }
    ret
  }
  if ((!is.matrix(A)) || (!is.numeric(A)))
    stop("argument must be a numeric matrix")
  n <- nrow(A)
  m <- ncol(A)
  i <- j <- 1
  L <- P <- diag(n)
  if (verbose){
  	cat("\nInitial equation:\n")
  	showEqn(A, b, simplify = TRUE, ...)
  }
  wasmissing <- missing(b)
  if(wasmissing) b <- matrix(0, n)
  else b <- as.matrix(b)
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
  rownames(A) <- NULL
  ret <- list(P=P, L=L, U=A)
  if (verbose && wasmissing){
  	cat("\nLower triangle equation:\n")
  	showEqn(L, simplify = TRUE, ...)
  	cat("\nUpper triangle equation:\n")
  	showEqn(ret$U, simplify = TRUE, ...)
  }
  if(!wasmissing){
  	if (verbose){
  		cat("\nLower triangle equation:\n")
  		showEqn(L, b, simplify = TRUE, ...)
  	}
    ret$d <- fbsolve(L, b, verbose=verbose)
    if (verbose){
    	cat(sprintf("\nIntermediate solution: d = (%s)\n", paste0(ret$d, collapse=', ')))
    	cat("\nUpper triangle equation:\n")
    	showEqn(ret$U, ret$d, simplify = TRUE, ...)
    }
    ret$x <- fbsolve(A, ret$d, verbose=verbose)
    if (verbose)
    	cat(sprintf("\nFinal solution: x = (%s)\n", paste0(ret$x, collapse=', ')))
  }
  ret <- lapply(ret, as.matrix)
  ret <- if (fractions) lapply(ret, MASS::fractions)
  else lapply(ret, function(x) round(x, round(abs(log(tol, 10)))))
  if(verbose) return(invisible(ret))
  ret
}
