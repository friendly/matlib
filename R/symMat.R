
# original from metaSEM::vec2symMat

#' Create a Symmetric Matrix from a Vector
#'
#' Creates a square symmetric matrix from a vector.
#'
#' @param x    A numeric vector used to fill the upper or lower triangle of the matrix.
#' @param diag Logical. If \code{TRUE} (the default), the diagonals of the created matrix are replaced
#'        by elements of x; otherwise, the diagonals of the created matrix are replaced by "1".
#' @param byrow Logical. If \code{FALSE} (the default), the created matrix is filled by columns;
#'        otherwise, the matrix is filled by rows.
#' @param names Either a logical or a character vector of names for the rows and columns of the matrix.
#'        If \code{FALSE}, no names are assigned; if \code{TRUE}, rows and columns are named
#'        \code{X1}, \code{X2}, ... .
#' @return A symmetric square matrix based on column major ordering of the elements in \code{x}.
#' @author Originally from \code{metaSEM::vec2symMat}, Mike W.-L. Cheung <mikewlcheung@nus.edu.sg>; modified by Michael Friendly
#' @export
#' @examples
#' symMat(1:6)
#' symMat(1:6, byrow=TRUE)
#' symMat(5:0, diag=FALSE)

symMat <-
function (x, diag = TRUE, byrow = FALSE, names = FALSE)
{
    m <- length(x)
    d <- if (diag)
        1
    else -1
    n <- floor((sqrt(1 + 8 * m) - d)/2)
    if (m != n * (n + d)/2)
        stop("Cannot make a square matrix as the length of \"x\" is incorrect.")
    mat <- diag(n)

	# fill the matrix
    if (byrow) {
        mat[upper.tri(mat, diag = diag)] <- x
        index <- lower.tri(mat)
        mat[index] <- t(mat)[index]
    }
    else {
        mat[lower.tri(mat, diag = diag)] <- x
        index <- upper.tri(mat)
        mat[index] <- t(mat)[index]
    }
	# handle names
    if (is.logical(names)) {
    	if (names) names <- paste("X", 1:n, sep = "")
    	else names <- NULL
    }
    else if (is.character(names)) {
    	if (length(names) !=n) {
    		warning("wrong number of variable names; ignored")
    		names <- NULL
    	}
    }
    if (!is.null(names)) rownames(mat) <- colnames(mat) <- names
    mat
}


