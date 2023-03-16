#' Gram-Schmidt Orthogonalization of a Matrix
#'
#' Carries out simple Gram-Schmidt orthogonalization of a matrix.
#' Treating the columns of the matrix \code{X} in the given order,
#' each successive column after the first is made orthogonal to all
#' previous columns by subtracting their projections on the current
#' column.
#'
#' @param X a matrix
#' @param normalize logical; should the resulting columns be normalized to unit length? The default is \code{TRUE}
#' @param verbose logical; if \code{TRUE}, print intermediate steps. The default is \code{FALSE}
#' @param tol the tolerance for detecting linear dependencies in the columns of a. The default is \code{sqrt(.Machine$double.eps)}
#' @param omit_zero_columns if \code{TRUE} (the default), remove linearly dependent columns from the result
#' @return A matrix of the same size as \code{X}, with orthogonal columns (but with 0 columns removed by default)
#' @author Phil Chalmers, John Fox
#' @export
#' @examples
#' (xx <- matrix(c( 1:3, 3:1, 1, 0, -2), 3, 3))
#' crossprod(xx)
#' (zz <- GramSchmidt(xx, normalize=FALSE))
#' zapsmall(crossprod(zz))
#'
#' # normalized
#' (zz <- GramSchmidt(xx))
#' zapsmall(crossprod(zz))
#'
#' # print steps
#' GramSchmidt(xx, verbose=TRUE)
#'
#' # A non-invertible matrix;  hence, it is of deficient rank
#' (xx <- matrix(c( 1:3, 3:1, 1, 0, -1), 3, 3))
#' R(xx)
#' crossprod(xx)
#' # GramSchmidt finds an orthonormal basis
#' (zz <- GramSchmidt(xx))
#' zapsmall(crossprod(zz))
#'
GramSchmidt <- function (X, normalize = TRUE, verbose = FALSE,
                         tol=sqrt(.Machine$double.eps), omit_zero_columns=TRUE) {
  B <- X
  B[, 2L:ncol(B)] <- 0
  if (verbose) {
    cat("\nInitial matrix:\n")
    printMatrix(X)
    cat("\nColumn 1: z1 = x1\n")
    printMatrix(B)
  }
  for (i in 2:ncol(X)) {
    res <- X[, i]
    for (j in 1:(i - 1)) res <- res - Proj(X[, i], B[, j])
    if (len(res) < tol) res <- 0
    B[, i] <- res
    if (verbose) {
      prj_char <- character(i - 1)
      for (j in 1:(i - 1)) prj_char[j] <- sprintf("Proj(x%i, z%i)",
                                                  i, j)
      cat(sprintf("\nColumn %i: z%i = x%i - %s\n", i, i,
                  i, paste0(prj_char, collapse = " - ")))
      printMatrix(B)
    }
  }
  zeros <- apply(B, 2, function(b) all(b == 0))
  if (omit_zero_columns && any(zeros)){
    B <- B[, !zeros]
    which_zeros <- (1:ncol(X))[zeros]
    message("linearly dependent column", if (length(which_zeros) > 1) "s" else "",
            " omitted: ", paste(which_zeros, collapse=", "))
  }
  if (normalize) {
    norm <- diag(1/len(B))
    colnames <- colnames(B)
    B <- B %*% norm
    colnames(B) <- colnames
    if (!omit_zero_columns && any(zeros)) B[, zeros] <- 0
    if (verbose) {
      cat("\nNormalized matrix: Z * inv(L) \n")
      printMatrix(B)
    }
  }
  if (verbose)
    return(invisible(B))
  B
}
