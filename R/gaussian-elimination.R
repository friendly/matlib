############################################################
# Gaussian Elimination functions, originally from John Fox
# https://stat.ethz.ch/pipermail/r-help/2007-September/140021.html
#############################################################

#' Gaussian Elimination
#'
#' \code{gaussianElimination} demonstrates the algorithm of row reduction used for solving
#' systems of linear equations of the form \eqn{A x = B}. Optional arguments \code{verbose}
#' and \code{fractions} may be used to see how the algorithm works.
#'
#' @param A coefficient matrix
#' @param B right-hand side vector or matrix. If \code{B} is a matrix, the result gives solutions for each column as the right-hand
#'        side of the equations with coefficients in \code{A}.
#' @param tol tolerance for checking for 0 pivot
#' @param verbose logical; if \code{TRUE}, print intermediate steps
#' @param fractions logical; if \code{TRUE}, try to express nonintegers as rational numbers
#' @return If \code{B} is absent, returns the reduced row-echelon form of \code{A}.
#'         If \code{B} is present, returns the reduced row-echelon form of \code{A}, with the
#'         same operations applied to \code{B}.
#' @author John Fox
#' @examples
#'   A <- matrix(c(2, 1, -1,
#'                -3, -1, 2,
#'                -2,  1, 2), 3, 3, byrow=TRUE)
#'   b <- c(8, -11, -3)
#'   gaussianElimination(A, b)
#'   gaussianElimination(A, b, verbose=TRUE, fractions=TRUE)
#'
#'
gaussianElimination <- function(A, B, tol=sqrt(.Machine$double.eps),
    verbose=FALSE, fractions=FALSE){
    # A: coefficient matrix
    # B: right-hand side vector or matrix
    # tol: tolerance for checking for 0 pivot
    # verbose: if TRUE, print intermediate steps
    # fractions: try to express nonintegers as rational numbers
    # If B is absent returns the reduced row-echelon form of A.
    # If B is present, reduces A to RREF carrying B along.
  if (fractions) {
    mass <- requireNamespace("MASS", quietly=TRUE)
    if (!mass) stop("fractions=TRUE needs MASS package")
  }
  if ((!is.matrix(A)) || (!is.numeric(A)))
        stop("argument must be a numeric matrix")
    n <- nrow(A)
    m <- ncol(A)
    if (!missing(B)){
        B <- as.matrix(B)
        if (!(nrow(B) == nrow(A)) || !is.numeric(B))
          stop("argument must be numeric and must match the number of row of A")
        A <- cbind(A, B)
        }
    i <- j <- 1
    while (i <= n && j <= m){
        while (j <= m){
            currentColumn <- A[,j]
            currentColumn[1:n < i] <- 0
            # find maximum pivot in current column at or below current row
            which <- which.max(abs(currentColumn))
            pivot <- currentColumn[which]
            if (abs(pivot) <= tol) { # check for 0 pivot
                j <- j + 1
                next
                }
            if (which > i) A[c(i, which),] <- A[c(which, i),]  # exchange rows
            A[i,] <- A[i,]/pivot            # pivot
            row <- A[i,]
            A <- A - outer(A[,j], row)      # sweep
            A[i,] <- row                    # restore current row
            if (verbose) cat("row:", i, "\n")
            if (verbose) if (fractions) print(MASS::fractions(A))
                else print(round(A, round(abs(log(tol,10)))))
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
    if (fractions) MASS::fractions (A) else round(A, round(abs(log(tol, 10))))
    }

#' Inverse of a Matrix
#'
#' Uses \code{\link{gaussianElimination}} to find the inverse of a square, non-singular matrix, \eqn{X}.
#'
#' The method is purely didactic: The identity matrix, \eqn{I}, is appended to \eqn{X}, giving
#' \eqn{X | I}.  Applying Gaussian elimination gives \eqn{I | X^{-1}}, and the portion corresponding
#' to \eqn{X^{-1}} is returned.
#'
#' @aliases inv
#' @param X a square numeric matrix
#' @param tol tolerance for checking for 0 pivot
#' @param ... other arguments passed on
#' @return the inverse of \code{X}
#' @author John Fox
#' @examples
#'   A <- matrix(c(2, 1, -1,
#'                -3, -1, 2,
#'                -2,  1, 2), 3, 3, byrow=TRUE)
#'   Inverse(A)
#'   Inverse(A, verbose=TRUE, fractions=TRUE)

Inverse <- function(X, tol=sqrt(.Machine$double.eps), ...){
    # returns the inverse of nonsingular X
    if ((!is.matrix(X)) || (nrow(X) != ncol(X)) || (!is.numeric(X)))
        stop("X must be a square numeric matrix")
    n <- nrow(X)
    X <- gaussianElimination(X, diag(n), tol=tol, ...) # append identity matrix
        # check for 0 rows in the RREF of X:
    if (any(apply(abs(X[,1:n]) <= sqrt(.Machine$double.eps), 1, all)))
        stop ("X is numerically singular")
    X[,(n + 1):(2*n)]  # return inverse
}
# synonym
inv <- function(X, ...) Inverse(X, tol=sqrt(.Machine$double.eps), ...)

#RREF <- function(X, ...) gaussianElimination(X, ...)
#    # returns the reduced row-echelon form of X


#' Echelon Form of a Matrix
#'
#' Returns the reduced row-echelon form of the matrix \code{X}, using \code{\link{gaussianElimination}}.
#' It is nothing more than a synonym for Gaussian elimination, but offers the possibility to show the
#' steps using \code{verbose=TRUE}.
#'
#' When the matrix \code{x} is square and non-singular, the result will be the identity matrix. Otherwise,
#' the result will have some all-zero rows, and the rank of the matrix is the number of not all-zero rows.
#'
#' @param X a matrix
#' @param ... other arguments passed to \code{gaussianElimination}
#' @return the reduced echelon form of \code{X}.
#' @author John Fox
#' @examples
#' A <- matrix(c(2, 1, -1,
#'              -3, -1, 2,
#'              -2,  1, 2), 3, 3, byrow=TRUE)
#' b <- c(8, -11, -3)
#' echelon(A, b, verbose=TRUE, fractions=TRUE)
#'
#' A <- matrix(c(1,2,3,4,5,6,7,8,10), 3, 3) # a nonsingular matrix
#' A
#' echelon(A) # the reduced row-echelon form of A
#'
#' b <- 1:3
#' echelon(A, b)  # solving the matrix equation Ax = b
#' echelon(A, diag(3)) # inverting A
#'
#' B <- matrix(1:9, 3, 3) # a singular matrix
#' B
#' echelon(B)
#' echelon(B, b)
#' echelon(B, diag(3))
#'

echelon <- function(X, ...) gaussianElimination(X, ...)

#' Generalized Inverse of a Matrix
#'
#' \code{Ginv} returns an arbitrary generalized inverse of the matrix \code{A}, using \code{gaussianElimination}.
#'
#' A generalized inverse is a matrix \eqn{\mathbf{A}^-} satisfying \eqn{\mathbf{A A^- A} = \mathbf{A}}.
#'
#' The purpose of this function is mainly to show how the generalized inverse can be computed using
#' Gaussian elimination.
#'
#' @param A numerical matrix
#' @param tol tolerance for checking for 0 pivot
#' @param verbose logical; if \code{TRUE}, print intermediate steps
#' @param fractions logical; if \code{TRUE}, try to express nonintegers as rational numbers
#' @return the generalized inverse of \code{A}, expressed as fractions if \code{fractions=TRUE}, or rounded
#' @seealso \code{\link[MASS]{ginv}} for a more generally usable function
#' @author John Fox
#'
#' @examples
#' A <- matrix(c(1,2,3,4,5,6,7,8,10), 3, 3) # a nonsingular matrix
#' A
#' Ginv(A, fractions=TRUE)  # a generalized inverse of A = inverse of A
#' round(Ginv(A) %*% A, 6)  # check
#'
#' B <- matrix(1:9, 3, 3) # a singular matrix
#' B
#' Ginv(B, fractions=TRUE)  # a generalized inverse of B
#' B %*% Ginv(B) %*% B   # check

#'

Ginv <- function(A, tol=sqrt(.Machine$double.eps), verbose=FALSE,
                 fractions=FALSE){
  # return an arbitrary generalized inverse of the matrix A
  # A: a matrix
  # tol: tolerance for checking for 0 pivot
  # verbose: if TRUE, print intermediate steps
  # fractions: try to express nonintegers as rational numbers
  if (fractions) {
    mass <- requireNamespace("MASS", quietly=TRUE)
    if (!mass) stop("fractions=TRUE needs MASS package")
  }
  m <- nrow(A)
  n <- ncol(A)
  B <- gaussianElimination(A, diag(m), tol=tol, verbose=verbose,
                           fractions=fractions)
  L <- B[,-(1:n)]
  AR <- B[,1:n]
  C <- gaussianElimination(t(AR), diag(n), tol=tol, verbose=verbose,
                           fractions=fractions)
  R <- t(C[,-(1:m)])
  AC <- t(C[,1:m])
  ginv <- R %*% t(AC) %*% L
  if (fractions) MASS::fractions (ginv) else round(ginv, round(abs(log(tol, 10))))
}

#' Cholesky Square Root of a Matrix
#'
#' Returns the Cholesky square root of the non-singular, symmetric matrix \code{X}.
#' The purpose is mainly to demonstrate the algorithm used by Kennedy & Gentle (1980).
#'
#' @param X a square symmetrix matrix
#' @param tol tolerance for checking for 0 pivot
#' @return the Cholesky square root of \code{X}
#' @references Kennedy W.J. Jr, Gentle J.E. (1980). \emph{Statistical Computing}. Marcel Dekker.
#' @seealso \code{\link[base]{chol}} for the base R function
#' @seealso \code{\link{gsorth}} for Gram-Schmidt orthogonalization of a data matrix
#' @author John Fox
#' @examples
#' C <- matrix(c(1,2,3,2,5,6,3,6,10), 3, 3) # nonsingular, symmetric
#' C
#' cholesky(C)
#' cholesky(C) %*% t(cholesky(C))  # check
#'

cholesky <- function(X, tol=sqrt(.Machine$double.eps)){
    # algorithm from Kennedy & Gentle (1980)
    if (!is.numeric(X)) stop("argument is not numeric")
    if (!is.matrix(X)) stop("argument is not a matrix")
    n <- nrow(X)
    if (ncol(X) != n) stop("matrix is not square")
    if (max(abs(X - t(X))) > tol) stop("matrix is not symmetric")
    D <- rep(0, n)
    L <- diag(n)
    i <- 2:n
    D[1] <- X[1, 1]
    if (abs(D[1]) < tol) stop("matrix is numerically singular")
    L[i, 1] <- X[i, 1]/D[1]
    for (j in 2:(n - 1)){
        k <- 1:(j - 1)
        D[j] <- X[j, j] - sum((L[j, k]^2) * D[k])
        if (abs(D[j]) < tol) stop("matrix is numerically singular")
        i <- (j + 1):n
        L[i, j] <- (X[i, j] -
                        colSums(L[j, k] * t(L[i, k, drop=FALSE]) * D[k]))/D[j]
        }
    k <- 1:(n - 1)
    D[n] <- X[n, n] - sum((L[n, k]^2) * D[k])
    if (abs(D[n]) < tol) stop("matrix is numerically singular")
    L %*% diag(sqrt(D))
    }
