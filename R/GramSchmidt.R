#' Gram-Schmidt Orthogonalization of a Matrix
#'
#' Carries out simple Gram-Schmidt orthogonalization of a matrix.
#' Treating the columns of the matrix \code{X} in the given order,
#' each successive column after the first is made orthogonal to all
#' previous columns by subtracting their projections on the current
#' column.
#'
#' @param X a matrix
#' @param normalize logical; should the resulting columns be normalized to unit length?
#' @return A matrix of the same size as \code{X}, with orthogonal columns
#' @author Phil Chalmers
#' @export
#' @examples
#' (xx <- matrix(c( 1:3, 3:1, 1, 0, -1), 3, 3))
#' crossprod(xx)
#' (yy <- GramSchmidt(xx))
#' zapsmall(crossprod(yy))

GramSchmidt <- function(X, normalize=TRUE){
  B <- X
  for(i in 2:ncol(X)){
  	res <- X[,i]
  	for(j in 1:(i-1))
  		 res <- res - Proj(X[,i],B[,j])
  	B[,i] <- res
  }
  if (normalize) {
	  norm <- diag(1/sqrt(colSums(B^2)))
	  B <- B %*% norm
  }
  B
}
