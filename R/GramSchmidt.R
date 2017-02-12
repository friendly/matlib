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
#' @param verbose logical; if \code{TRUE}, print intermediate steps
#' @return A matrix of the same size as \code{X}, with orthogonal columns
#' @author Phil Chalmers
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
#' # non-invertible matrix; hence, its basis is not orthonormal
#' (xx <- matrix(c( 1:3, 3:1, 1, 0, -1), 3, 3))
#' crossprod(xx)
#' (zz <- GramSchmidt(xx))
#' zapsmall(crossprod(zz))
#' 
GramSchmidt <- function(X, normalize=TRUE, verbose=FALSE){
  B <- X
  B[,2L:ncol(B)] <- 0
  if (verbose){
  	cat("\nInitial matrix:\n")
  	printMatrix(X)
  	cat("\nColumn 1: z1 = x1\n")
  	printMatrix(B)
  }
  for(i in 2:ncol(X)){
  	res <- X[,i]
  	for(j in 1:(i-1))
  		 res <- res - Proj(X[,i],B[,j])
  	B[,i] <- res
  	if (verbose){
  		prj_char <- character(i-1)
  		for(j in 1:(i-1))
  			prj_char[j] <- sprintf('Proj(x%i, z%i)', i, j)
  		cat(sprintf("\nColumn %i: z%i = x%i - %s\n", 
  					i, i, i, paste0(prj_char, collapse=' - ')))
  		printMatrix(B)
  	}
  }
  if (normalize) {
	  norm <- diag(1/len(B))
	  B <- B %*% norm
	  if (verbose){
	  	cat("\nNormalized matrix: Z * inv(L) \n")
	  	printMatrix(B)
	  }
  }
  if(verbose) return(invisible(B))
  B
}
