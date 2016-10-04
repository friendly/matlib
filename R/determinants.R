##################################
# determinants: minor and cofactor
##################################

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
#' @examples
#' M <- matrix(c(4, -12, -4,
#'               2,   1,  3,
#'              -1,  -3,  2), 3, 3, byrow=TRUE)
#' minor(M, 1, 1)
#' minor(M, 1, 2)
#' minor(M, 1, 3)

minor <- function(A, i, j) {
  det(A[-i, -j])
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
#' Returns the vector of cofactors of row i of the square matrix A.  The determinant, \code{det(A)},
#' can then be found as \code{M[i,] \%*\% rowCofactors(M,i)} for any row, i.
#'
#' @param A a square matrix
#' @param i row index
#' @return a vector of the cofactors of A[i,]
#' @family determinants
#' @seealso \code{\link[base]{det}} for the determinant
#' @author Michael Friendly
#' @examples
#' M <- matrix(c(4, -12, -4,
#'               2,   1,  3,
#'              -1,  -3,  2), 3, 3, byrow=TRUE)
#' minor(M, 1, 1)
#' minor(M, 1, 2)
#' minor(M, 1, 3)
#' rowCofactors(M, 1)
#' det(M)
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

