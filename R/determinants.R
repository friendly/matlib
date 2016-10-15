##################################
# determinants: minor and cofactor
##################################

#' Determinant of a Square Matrix
#'
#' Returns the determinant of a square matrix \code{X},
#' computed either by Gaussian elimination, expansion by cofactors, or as the product of the eigenvalues of the matrix.
#' If the latter, \code{X} must be symmetric.
#'
#' @param X a square matrix
#' @param method one of `"elimination"` (the default), `"eigenvalues"`, or `"cofactors"` (for computation by minors and cofactors)
#' @param verbose logical; if \code{TRUE}, print intermediate steps
#' @param fractions logical; if \code{TRUE}, try to express non-integers as rational numbers
#' @param ... arguments passed to \code{\link{gaussianElimination}} or \code{\link{Eigen}}
#' @return the determinant of \code{X}
#' @family determinants
#' @seealso \code{\link[base]{det}} for the base R function
#' @seealso \code{\link{gaussianElimination}}, \code{\link{Eigen}}
#' @author John Fox
#' @export
#' @examples
#' A <- matrix(c(1,2,3,2,5,6,3,6,10), 3, 3) # nonsingular, symmetric
#' A
#' Det(A)
#' Det(A, verbose=TRUE, fractions=TRUE)
#' B <- matrix(1:9, 3, 3) # a singular matrix
#' B
#' Det(B)
#' C <- matrix(c(1, .5, .5, 1), 2, 2) # square, symmetric, nonsingular
#' Det(C)
#' Det(C, method="eigenvalues")
#' Det(C, method="cofactors")

Det <- function(X, method=c("elimination", "eigenvalues", "cofactors"), verbose=FALSE, fractions=FALSE, ...){
    if (length(X) == 1) return(X)
    if (!(is.matrix(X) && nrow(X) == ncol(X))) stop("X must be a square matrix")
    method <- match.arg(method)
    if (method == "elimination"){
      res <- gaussianElimination(X, verbose=verbose, fractions=fractions, ...)
      pivots <- attr(res, "pivots")
      interchanges <- attr(res, "interchanges")
      det <- attr(res, "det")
      if (verbose){
        if (fractions){
          pivots <- MASS::fractions(pivots)
          det <- MASS::fractions(det)
        }
        else {
          pivots <- signif(pivots)
          det <- signif(det)
        }
        cat(paste0("\n det = (-1)^", interchanges, " x ", paste(pivots, collapse=" x "), " = ", det, "\n"))
        return(invisible(attr(res, "det")))
      }
      else return(attr(res, "det"))
    }
    else if (method == "eigenvalues"){
      values <- Eigen(X, ...)$values
      det <- prod(values)
      if (verbose){
        values0 <- values
        det0 <- det
        if (fractions){
          det0 <- MASS::fractions(det)
          values0 <- MASS::fractions(values)
        }
        else {
          det0 <- signif(det)
          values0 <- signif(values)
        }
        cat(paste0("\n det = ", paste(values0, collapse=" x "), " = ", det0, "\n"))
        return(invisible(det))
      }
      else return(det)
    }
    else {
        as.vector(X[1,] %*% rowCofactors(X, 1))
    }
}


#' Minor of A[i,j]
#'
#' Returns the minor of element (i,j) of the square matrix A, i.e., the determinant of the
#' sub-matrix that results when row i and column j are deleted.
#'
#' @param A a square matrix
#' @param i row index
#' @param j column index
#' @return the minor of A[i,j]
#' @family determinants
#' @seealso \code{\link{rowMinors}} for all minors of a given row
#' @author Michael Friendly
#' @export
#' @examples
#' M <- matrix(c(4, -12, -4,
#'               2,   1,  3,
#'              -1,  -3,  2), 3, 3, byrow=TRUE)
#' minor(M, 1, 1)
#' minor(M, 1, 2)
#' minor(M, 1, 3)

minor <- function(A, i, j) {
  Det(A[-i, -j], method="cofactors")
}

#' Cofactor of A[i,j]
#'
#' Returns the cofactor of element (i,j) of the square matrix A, i.e., the signed minor of the
#' sub-matrix that results when row i and column j are deleted.
#'
#' @param A a square matrix
#' @param i row index
#' @param j column index
#' @return the cofactor of A[i,j]
#' @family determinants
#' @seealso \code{\link{rowCofactors}} for all cofactors of a given row
#' @author Michael Friendly
#' @export
#' @examples
#' M <- matrix(c(4, -12, -4,
#'               2,   1,  3,
#'              -1,  -3,  2), 3, 3, byrow=TRUE)
#' cofactor(M, 1, 1)
#' cofactor(M, 1, 2)
#' cofactor(M, 1, 3)
#'

cofactor <- function(A, i, j) {
  m <- minor(A, i, j)
  (-1)^(i+j) * m
}

#' Row Minors of A[i,]
#'
#' Returns the vector of minors of row i of the square matrix A
#'
#' @param A a square matrix
#' @param i row index
#' @return a vector of the minors of A[i,]
#' @family determinants
#' @author Michael Friendly
#' @export
#' @examples
#' M <- matrix(c(4, -12, -4,
#'               2,   1,  3,
#'              -1,  -3,  2), 3, 3, byrow=TRUE)
#' minor(M, 1, 1)
#' minor(M, 1, 2)
#' minor(M, 1, 3)
#' rowMinors(M, 1)

rowMinors <- function(A, i) {
  res <- numeric(ncol(A))
  for(j in 1:ncol(A)) {
    res[j] <- minor(A, i, j)
  }
  res
}

#' Row Cofactors of A[i,]
#'
#' Returns the vector of cofactors of row i of the square matrix A.  The determinant, \code{Det(A)},
#' can then be found as \code{M[i,] \%*\% rowCofactors(M,i)} for any row, i.
#'
#' @param A a square matrix
#' @param i row index
#' @return a vector of the cofactors of A[i,]
#' @family determinants
#' @seealso \code{\link{Det}} for the determinant
#' @author Michael Friendly
#' @export
#' @examples
#' M <- matrix(c(4, -12, -4,
#'               2,   1,  3,
#'              -1,  -3,  2), 3, 3, byrow=TRUE)
#' minor(M, 1, 1)
#' minor(M, 1, 2)
#' minor(M, 1, 3)
#' rowCofactors(M, 1)
#' Det(M)
#' # expansion by cofactors of row 1
#' M[1,] %*% rowCofactors(M,1)
#'

rowCofactors <- function(A, i) {
  res <- numeric(ncol(A))
  for(j in 1:ncol(A)) {
    res[j] <- cofactor(A, i, j)
  }
  res
}

