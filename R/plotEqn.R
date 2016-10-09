#' Plot Linear Equations
#'
#' Shows what matrices \eqn{A, b} look like as the system of linear equations, \eqn{A x = b} with two unknowns,
#' x1, x2, by plotting a line for each equation.
#'
#' @param A either the matrix of coefficients of a system of linear equations, or the matrix \code{cbind(A,b)}.
#'        The \code{A} matrix must have two columns.
#' @param b if supplied, the vector of constants on the right hand side of the equations, of length matching
#'        the number of rows of \code{A}.
#' @param vars a numeric or character vector of names of the variables.
#'        If supplied, the length must be equal to the number of unknowns in the equations.
#'        The default is \code{paste0("x", 1:ncol(A)}.
#' @param xlim horizontal axis limits for the first variable
#' @param ylim vertical axis limits for the second variable; if missing, \code{ylim} is calculated from the
#'        range of the set of equations over the \code{xlim}.
#' @param col scalar or vector of colors for the lines, recycled as necessary
#' @param lwd scalar or vector of line widths for the lines, recycled as necessary
#' @param lty scalar or vector of line types for the lines, recycled as necessary
#' @param axes logical; draw horizontal and vertical axes through (0,0)?
#' @param labels logical, or a vector of character labels for the equations; if \code{TRUE}, each equation is labeled
#'      using the character string resulting from \code{\link{showEqn}}
#' @param solution logical; should the solution points for pairs of equations be marked?
#' @return nothing; used for the side effect of making a plot
#'
#' @author Michael Friendly
#' @references Fox, J. and Friendly, M. (2016). "Visualizing Simultaneous Linear Equations, Geometric Vectors, and
#' Least-Squares Regression with the matlib Package for R". \emph{useR Conference}, Stanford, CA, June 27 - June 30, 2016.
#' @importFrom graphics abline lines plot text points
#' @seealso \code{\link{showEqn}}

#' @examples
#' # consistent equations
#' A<- matrix(c(1,2,3, -1, 2, 1),3,2)
#' b <- c(2,1,3)
#' showEqn(A, b)
#' plotEqn(A,b)
#'
#' # inconsistent equations
#' b <- c(2,1,6)
#' showEqn(A, b)
#' plotEqn(A,b)


plotEqn <- function(A, b, vars, xlim=c(-4, 4), ylim,
				col=1:nrow(A), lwd=2, lty=1,
				axes=TRUE, labels=TRUE,
				solution=TRUE
				) {
  if (!is.numeric(A) || !is.matrix(A)) stop("A must be a numeric matrix")
  if (missing(b)) {
    b <- A[,ncol(A)]   # assume last column of Ab
    A <- A[,-ncol(A)]  # remove b from A
  }
	if (ncol(A) != 2) stop("plotEqn only handles two-variable equations. Use plotEqn3d for three-variable equations.")
  if (missing(vars)) vars <- paste0("x", 1:ncol(A))

	# set values for horizontal variable
	x <- seq(xlim[1], xlim[2], length=10)

	neq <- nrow(A)
	if (length(col) < neq) col <- rep_len(col, length.out=neq)
	if (length(lwd) < neq) lwd <- rep_len(lwd, length.out=neq)
	if (length(lty) < neq) lty <- rep_len(lty, length.out=neq)

  if (missing(ylim)) {
    ylim <- c(0, 0)
    for (i in 1:neq) {
      if (A[i,2] != 0) {
        y <- (b[i] - A[i,1] * x) / A[i,2]
        ylim <- range(c(ylim, y))
      }
    }
  }

	if (is.logical(labels) && labels) {
    labels <- showEqn(A,b, vars, simplify=TRUE)
	}
	else labels=NULL

	for (i in 1:neq) {
	  if (i==1) plot(xlim, ylim, type="n", xlab = vars[1], ylab = vars[2], xlim = xlim, ylim = ylim)

	  if (A[i,2] == 0) abline( v = b[i] / A[i,1], col = col[i], lwd = lwd[i], lty = lty[i] )
	  else {
	    # calculate y values for current equation
	    y <- (b[i] - A[i,1] * x) / A[i,2]
	    lines( x, y, col = col[i], type = 'l', lwd = lwd[i], lty = lty[i] )
	  }

	  if (!is.null(labels)) {
	    xl <- x[1]
	    yl <- y[1]
	    text(xl, yl, labels[i], col=col[i], pos=4)
	  }
	}
	if (axes) abline(h=0, v=0, col="gray")

	if (solution) {
	  for (i in 1:neq-1) {
	    for (j in i:neq) {
	      x <- try(solve(A[c(i,j),],b[c(i,j)]), silent=TRUE)
	      if (!inherits(x, "try-error")) points(x[1], x[2], cex=1.5)
	    }
	  }
	}
}


#' Plot Linear Equations in 3D
#'
#' Shows what matrices \eqn{A, b} look like as the system of linear equations, \eqn{A x = b} with three unknowns,
#' x1, x2, and x3, by plotting a plane for each equation.

#' @param A either the matrix of coefficients of a system of linear equations, or the matrix \code{cbind(A,b)}
#'        The \code{A} matrix must have three columns.
#' @param b if supplied, the vector of constants on the right hand side of the equations, of length matching
#'        the number of rows of \code{A}.
#' @param vars a numeric or character vector of names of the variables.
#'        If supplied, the length must be equal to the number of unknowns in the equations.
#'        The default is \code{paste0("x", 1:ncol(A)}.
#' @param xlim axis limits for the first variable
#' @param ylim axis limits for the second variable
#' @param zlim horizontal axis limits for the second variable; if missing, \code{zlim} is calculated from the
#'        range of the set of equations over the \code{xlim} and \code{ylim}
#' @param col scalar or vector of colors for the lines, recycled as necessary
#' @param alpha transparency applied to each plane
#' @param labels logical, or a vector of character labels for the equations; not yet implemented.
#' @param solution logical; should the solution point for all equations be marked (if possible)
#' @param axes logical; whether to frame the plot with coordinate axes
#' @param lit logical, specifying if lighting calculation should take place on geometry; see \code{\link[rgl]{rgl.material}}
#'
#' @return nothing; used for the side effect of making a plot
#'
#' @author Michael Friendly, John Fox
#' @references Fox, J. and Friendly, M. (2016). "Visualizing Simultaneous Linear Equations, Geometric Vectors, and
#' Least-Squares Regression with the matlib Package for R". \emph{useR Conference}, Stanford, CA, June 27 - June 30, 2016.
#' @examples
#' # three consistent equations in three unknowns
#' A <- matrix(c(13, -4, 2, -4, 11, -2, 2, -2, 8), 3,3)
#' b <- c(1,2,4)
#' plotEqn3d(A,b)

plotEqn3d <- function( A, b, vars, xlim=c(-2,2), ylim=c(-2,2), zlim,
                       col=2:(nrow(A)+1), alpha=1, labels=FALSE, solution=TRUE,
                       axes=TRUE, lit=FALSE)
{
  if (!is.numeric(A) || !is.matrix(A)) stop("A must be a numeric matrix")
  if (missing(b)) {
    b <- A[,ncol(A)]   # assume last column of Ab
    A <- A[,-ncol(A)]  # remove b from A
  }
  if (ncol(A) != 3) stop("plotEqn3d only handles three-variable equations")
  if (missing(vars)) vars <- paste0("x", 1:ncol(A))

  neq <- nrow(A)
  # determine zlim if not specified
  if (missing(zlim)) {
    x <- xlim; y <- ylim
    zlim <- c(0, 0)
    for (i in 1:neq) {
      if (A[i,3] != 0) {
        z <- (b[i] - A[i,1] * x - A[i,2] * y) / A[i,3]
        zlim <- range(c(zlim, z))
      }
    }
  }

  if (length(col) < neq) col <- rep_len(col, length.out=neq)

  if (is.logical(labels) && labels) {
#    labels <- showEqn(A,b, vars)
    labels <- paste0("(", 1:neq, ")")
  }
  else labels=NULL

  # Initialize the scene, no data plotted
  # Create some dummy data
  dat <- replicate(2, 1:3)
  rgl::plot3d(dat, type = 'n', xlim = xlim, ylim = ylim, zlim = c(-3, 3),
              xlab = vars[1], ylab = vars[2], zlab = vars[3],
              axes=axes)
  # Add planes
  rgl::planes3d(A[,1], A[,2], A[,3], -b, col=col, alpha=alpha, lit=lit)

  # show the solution??
  if (solution) {
    x <- try(solve(A,b), silent=TRUE)
    if (!inherits(x, "try-error")) rgl::spheres3d(solve(A,b), radius=0.2)
  }

#   if (!is.null(labels)) {
#     for (i in 1:neq) {
#       xl <- xlim[1]
#       yl <- ylim[1]
#       zl <- if (A[i,3] != 0) min((b[i] - A[i,1] * xl - A[i,2] * yl) / A[i,3]) else 0
#       rgl::text3d(xl, yl, zl, labels[i])
#     }
#   }
}
