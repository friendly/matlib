#####################################################

#' Plot Linear Equations

#' Shows what matrices \eqn{A, b} look like as the system of linear equations, \eqn{A x = b},
#' by plotting a line for each equation.
#'
#' @param A either the matrix of coefficients of a system of linear equations, or the matrix \code{cbind(A,b)}
#' @param b if supplied, the vector of constants on the right hand side of the equations
#' @param vars a numeric or character vector of names of the variables.
#'        If supplied, the length must be equal to the number of unknowns in the equations.
#'        The default is \code{paste0("x", 1:ncol(A)}.
#' @param xlim horizontal axis limits for the first variable
#' @param ylim horizontal axis limits for the second variable
#' @param col scalar or vector of colors for the lines, recycled as necessary
#' @param lwd scalar or vector of line widths for the lines, recycled as necessary
#' @param lty scalar or vector of line types for the lines, recycled as necessary
#' @param axes logical; draw horizontal and vertical axes through (0,0)?
#' @param labels logical, or a vector of character labels for the equations
#' @examples
#' # consistent equations
#' A<- matrix(c(1,2,3, -1, 2, 1),3,2)
#' b <- c(2,1,3)
#' showeqn(A, b)
#' plotEqn(A,b)
#'
#' # inconsistent equations
#' b <- c(2,1,6)
#' showeqn(A, b)
#' plotEqn(A,b)


plotEqn <- function(A, b, vars, xlim=c(-4, 4), ylim,
				col=1:nrow(A), lwd=2, lty=1,
				axes=TRUE, labels=TRUE) {
  if (!is.numeric(A) || !is.matrix(A)) stop("A must be a numeric matrix")
  if (missing(b)) {
    b <- A[,ncol(A)]   # assume last column of Ab
    A <- A[,-ncol(A)]  # remove b from A
  }
	if (ncol(A) != 2) stop("plotEqn only handles two-variable equations")
  if (missing(vars)) vars <- paste0("x", 1:ncol(A))

	# set values for horizontal variable
	x <- seq(xlim[1], xlim[2], length=10)

	neq <- nrow(A)
	if (length(col) < neq) col <- rep_len(col, length=neq)
	if (length(lwd) < neq) lwd <- rep_len(lwd, length=neq)
	if (length(lty) < neq) lty <- rep_len(lty, length=neq)

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
    labels <- showEqn(A,b, vars)
	}
	else labels=NULL

		for (i in 1:nrow(A)) {
	  # calculate y values for current equation
	  # if A[i,2]==0 this will give Inf, so have to use abline()
	  y <- (b[i] - A[i,1] * x) / A[i,2]
	  if (i == 1)
	    # FIXME: this will fail if A[1,2]==0
	    plot(
	      x, y, type = 'l', col = col[i], lwd = lwd[i], lty = lty[i],
	      xlab = vars[1], ylab = vars[2], xlim = xlim, ylim = ylim
	    )
	  else
	  if (A[i,2] == 0)
	      # a vertical line
	      abline( v = b[i] / A[i,1], col = col[i], lwd = lwd[i], lty = lty[i] )
	  else
	    lines( x, y, col = col[i], type = 'l', lwd = lwd[i], lty = lty[i] )

	  if (!is.null(labels)) {
	    xl <- x[1]
	    yl <- y[1]
	    text(xl, yl, labels[i], col=col[i], pos=4)
	  }
	}
	if (axes) abline(h=0, v=0, col="gray")
}


