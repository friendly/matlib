#' Demonstrate the SVD for a 3 x 3 matrix
#'
#' This function draws an \code{rgl} scene consisting of a representation of the identity matrix and a
#' 3 x 3 matrix \code{A}, together with the corresponding representation of the
#' matrices U, D, and V in the SVD decomposition,
#' A = U D V'.
#'
#' @param A      A 3 x 3 numeric matrix
#' @param shape  Basic shape used to represent the identity matrix: \code{"cube"} or \code{"sphere"}
#' @param alpha  transparency value used to draw the shape
#' @param col    Vector of 6 colors for the faces of the basic cube
#'
#' @return Nothing
#' @export
#' @author Original idea from Duncan Murdoch
#' @importFrom grDevices rainbow
#' @export
#' @examples
#' A <- matrix(c(1,2,0.1, 0.1,1,0.1, 0.1,0.1,0.5), 3,3)
#' svdDemo(A)
#'
#' \dontrun{
#' B <- matrix(c( 1, 0, 1, 0, 2, 0,  1, 0, 2), 3, 3)
#' svdDemo(B)
#'
#' # a positive, semi-definite matrix with eigenvalues 12, 6, 0
#' C <- matrix(c(7, 4, 1,  4, 4, 4,  1, 4, 7), 3, 3)
#' svdDemo(C)
#' }
#'
svdDemo <- function(A, shape=c("cube", "sphere"), alpha=0.7,  col=rainbow(6)) {

	d <- dim(A)
	if( (length(d) != 2 ) | !all(d==3) ) stop("argument must be a 3 x 3 matrix")

	svd <- svd(A)
	U <- svd$u
	D <- diag(svd$d)
	V <- svd$v

	shape <- match.arg(shape)

	if (shape=="cube")
		Shape <- cube3d(color=rep(col, each = 4), alpha=alpha)
	else Shape <- subdivision3d(cube3d(color=rep(col, rep(4*4^4,6)), alpha=alpha), depth=4)

	axes <- rbind( diag(3), -diag(3) )
	rownames(axes) <- c("x", "y", "z", rep(" ", 3))

	mult <- function(matrix, obj) transform3d(obj, t(matrix))

	open3d()
	# define the layout of subscenes
	mat <- matrix(1:6, 2, 3)
	layout3d(rbind(mat, mat+6), heights = c(3,1,3,1), sharedMouse=TRUE)

	shade3d(Shape)
	vectors3d(axes, frac.lab=1.2, headlength = 0.2, radius=1/20, lwd=3)

	next3d()
	text3d(0,0,0, "Identity", cex=1.5)
	next3d()
	next3d(reuse=FALSE)
	next3d(reuse=FALSE)
	shade3d(mult(A, Shape))
	vectors3d(axes, frac.lab=1.2, headlength = 0.2, radius=1/20, lwd=3)
	next3d()
	text3d(0,0,0,"A", cex=1.5)

	next3d()
	shade3d(mult(U, Shape))
	vectors3d(axes, frac.lab=1.2, headlength = 0.2, radius=1/20, lwd=3)
	next3d()
	text3d(0,0,0, "U", cex=1.5)

	next3d()
	shade3d(mult(D, Shape))
	vectors3d(axes, frac.lab=1.2, headlength = 0.2, radius=1/20, lwd=3)
	next3d()
	text3d(0,0,0, "D", cex=1.5)

	next3d()
	shade3d(mult(V, Shape))
	vectors3d(axes, frac.lab=1.2, headlength = 0.2, radius=1/20, lwd=3)
	next3d()
	text3d(0,0,0, "V", cex=1.5)
}
