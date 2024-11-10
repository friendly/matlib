#' Position of a point along a line
#'
#' A utility function for drawing vector diagrams. Find position of an interpolated point along a line from \code{x1} to \code{x2}.
#'
#' The function takes a step of length \code{d} along the line defined by the difference between the two points, \code{x2 - x1}.
#' When \code{absolute=FALSE}, this step is proportional to the difference,
#' while when \code{absolute=TRUE}, the difference is first scaled to unit length so that the step is always of length \code{d}.
#' Note that the physical length of a line in different directions in a graph depends on the aspect ratio of the plot axes,
#' and lines of the same length will only appear equal if the aspect ratio is one
#' (\code{asp=1} in 2D, or \code{aspect3d("iso")} in 3D).
#'
#' @param x1  A vector of length 2 or 3, representing the starting point of a line in  2D or 3D space
#' @param x2  A vector of length 2 or 3, representing the ending point of a line in  2D or 3D space
#' @param d   The distance along the line from \code{x1} to \code{x2} of the point to be found.
#' @param absolute logical; if \code{TRUE}, \code{d} is taken as an absolute distance along the line; otherwise it
#'            is calculated as a relative distance, i.e., a fraction of the length of the line.
#' @return The interpolated point, a vector of the same length as \code{x1}
#' @family vector diagrams
#' @export
#' @examples
#' x1 <- c(0, 0)
#' x2 <- c(1, 4)
#' pointOnLine(x1, x2, 0.5)
#' pointOnLine(x1, x2, 0.5, absolute=FALSE)
#' pointOnLine(x1, x2, 1.1)
#'
#' y1 <- c(1, 2, 3)
#' y2 <- c(3, 2, 1)
#' pointOnLine(y1, y2, 0.5)
#' pointOnLine(y1, y2, 0.5, absolute=FALSE)

pointOnLine <- function(x1, x2, d, absolute=TRUE) {
	v <- x2 - x1
	if (absolute) v <- v / len(v)
	x1 + d * v
}


#' Draw a corner showing the angle between two vectors
#'
#' A utility function for drawing vector diagrams. Draws two line segments to indicate the angle between two vectors,
#' typically used for indicating orthogonal vectors are at right angles in 2D and 3D diagrams.
#'
#' In this implementation, the two vectors are specified by three points, \code{p1}, \code{p2}, \code{p3}, meaning
#' a line from \code{p1} to \code{p2}, and another line from \code{p2} to \code{p3}.
#'
#' @param p1 Starting point of first vector
#' @param p2 End point of first vector, and also start of second vector
#' @param p3 End point of second vector
#' @param d  The distance from \code{p2} along each vector for drawing their corner
#' @param absolute logical; if \code{TRUE}, \code{d} is taken as an absolute distance along the vectors; otherwise it
#'            is calculated as a relative distance, i.e., a fraction of the length of the vectors.
#'            See \code{\link{pointOnLine}} for the precise definition.
#' @param ... Arguments passed to \code{\link[graphics]{lines}} or to \code{\link[rgl]{lines3d}}
#' @export
#' @return none
#' @family vector diagrams
#'
#' @examples
#' # none yet
corner <- function(p1, p2, p3, d=.10, absolute=TRUE, ...) {
#  lens <- lengths(list(p1, p2, p3))
  lens <- sapply(list(p1, p2, p3), length)
  if (!all(diff(lens) ==0)) stop("Arguments p1, p2, p3 must be of the same length")
  if (any(lens < 2) | any(lens > 3)) stop("Only works for 2D or 3D")

	p21 <- pointOnLine(p2, p1, d=d, absolute=absolute)
	p23 <- pointOnLine(p2, p3, d=d, absolute=absolute)
  p123 <- p2 + (p23-p2) + (p21-p2)
#	segments3d(rbind(p21, p123, p123, p23), ...)
  pts <- rbind(p21, p123, p23)
	if (length(p1)==2)
		lines(pts, ...)
	else lines3d(pts,  ...)
}


#' Draw an arc showing the angle between vectors
#'
#' A utility function for drawing vector diagrams. Draws a circular arc to show the angle between two vectors in 2D or 3D.
#'
#'
#' In this implementation, the two vectors are specified by three points, \code{p1}, \code{p2}, \code{p3}, meaning
#' a line from \code{p1} to \code{p2}, and another line from \code{p2} to \code{p3}.
#'
#' @param p1 Starting point of first vector
#' @param p2 End point of first vector, and also start of second vector
#' @param p3 End point of second vector
#' @param d  The distance from \code{p2} along each vector for drawing their corner
#' @param absolute logical; if \code{TRUE}, \code{d} is taken as an absolute distance along the vectors; otherwise it
#'            is calculated as a relative distance, i.e., a fraction of the length of the vectors.
#' @param ... Arguments passed to \code{\link[graphics]{lines}} or to \code{\link[rgl]{lines3d}}
#' @return none
#' @references \url{https://math.stackexchange.com/questions/1507248/find-arc-between-two-tips-of-vectors-in-3d}
#' @family vector diagrams
#' @export
#' @examples
#' library(rgl)
#' vec <- rbind(diag(3), c(1,1,1))
#' rownames(vec) <- c("X", "Y", "Z", "J")
#' open3d()
#' aspect3d("iso")
#' vectors3d(vec, col=c(rep("black",3), "red"), lwd=2)
#' # draw the XZ plane, whose equation is Y=0
#' planes3d(0, 0, 1, 0, col="gray", alpha=0.2)
#' # show projections of the unit vector J
#' segments3d(rbind( c(1,1,1), c(1, 1, 0)))
#' segments3d(rbind( c(0,0,0), c(1, 1, 0)))
#' segments3d(rbind( c(1,0,0), c(1, 1, 0)))
#' segments3d(rbind( c(0,1,0), c(1, 1, 0)))
#' segments3d(rbind( c(1,1,1), c(1, 0, 0)))
#'
#' # show some orthogonal vectors
#' p1 <- c(0,0,0)
#' p2 <- c(1,1,0)
#' p3 <- c(1,1,1)
#' p4 <- c(1,0,0)
#' # show some angles
#' arc(p1, p2, p3, d=.2)
#' arc(p4, p1, p2, d=.2)
#' arc(p3, p1, p2, d=.2)

arc <- function (p1, p2, p3, d=.10, absolute=TRUE, ... ) {
  A <- pointOnLine(p2, p1, d=d, absolute=absolute)
  B <- pointOnLine(p2, p3, d=d, absolute=absolute)
  O <- p2
  phi <- acos(crossprod( A-O, B-O ))
  t <- seq(0, 1, length.out=10)
  pts <- matrix(0, nrow=length(t), ncol=length(p2))
  for (i in seq_along(t)) {
    pts[i,] <- O +
      as.vector(sin( (1-t[i])*phi ) / sin(phi)) * (A-O) +
      as.vector(sin( t[i]*phi ) / sin(phi)) * (B-O)
  }
  if (length(p1)==2)
    lines(pts, ...)
  else lines3d(pts,  ...)
}






