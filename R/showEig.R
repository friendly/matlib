#' Show the eigenvectors associated with a covariance matrix
#'
#' This function is designed for illustrating the eigenvectors associated with the
#' covariance matrix for a given bivariate data set.  It draws a data ellipse of
#' the data and adds vectors showing the eigenvectors of the covariance matrix.
#'
#' @param X           A two-column matrix or data frame
#' @param col.vec     color for eigenvectors
#' @param lwd.vec     line width for eigenvectors
#' @param mult        length multiplier(s) for eigenvectors
#' @param asp         aspect ratio of plot, set to \code{asp=1} by default, and passed to dataEllipse
#' @param levels      passed to dataEllipse determining the coverage of the data ellipse(s)
#' @param plot.points logical; should the points be plotted?
#' @param add         logical; should this call add to an existing plot?
#' @param ...         other arguments passed to \code{link[car]{dataEllipse}}
#' @author Michael Friendly
#' @seealso \code{\link[car]{dataEllipse}}
#' @importFrom car dataEllipse
#' @export
#' @examples
#' x <- rnorm(200)
#' y <- .5 * x + .5 * rnorm(200)
#' X <- cbind(x,y)
#' showEig(X)
#'
#' # Duncan data
#' data(Duncan, package="carData")
#' showEig(Duncan[, 2:3], levels=0.68)
#' showEig(Duncan[,2:3], levels=0.68, robust=TRUE, add=TRUE, fill=TRUE)


showEig <-
	function(X,
					 col.vec = "blue",
					 lwd.vec = 3,
					 mult = sqrt(qchisq(levels, 2)),
					 asp = 1,
           levels=c(0.5, 0.95),
           plot.points=TRUE,
           add = !plot.points,
           ...)
{
	X <- as.matrix(X)
  if (!is.numeric(X)) stop("X must be numeric")
	if (p <- ncol(X) > 2) {
		warning(paste("'X' has", p, "columns. Only the first two are used"))
		X <- X[, 1:2]
	}

	n <- dim(X)[1]
	mu <- colMeans(X)
	Sigma <- var(X)
	ev <-Eigen(Sigma)
	vals <- ev$values
	vecs <- ev$vectors
	r <- cor(X[,1], X[,2])
	if (r > 0 && all(vecs[, 1] < 0)) mult <- -1 * mult

	dataEllipse(X, asp=asp, levels=levels, plot.points=plot.points, add=add, ...)

	if( all(abs(mult) > 1e-5) ) {
		for (j in 1:2) {
		arrows(mu[1], mu[2],
					mu[1] + mult * vecs[1,j]*sqrt(vals[j]),
					mu[2] + mult * vecs[2,j]*sqrt(vals[j]),
					length=.1, angle=18, col=col.vec, lwd=lwd.vec)
		}
	}
	invisible(ev)
}
