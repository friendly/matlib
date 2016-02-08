
#' Draw 3D vectors
#'
#' This function draws vectors in a 3D plot, in a way that facilitates constructing vector diagrams. It allows vectors to be
#' specified as rows of a matrix, and can draw labels on the vectors.
#'
#' @section Bugs:
#' At present, the color (\code{color=}) argument is not handled as expected when more than one vector is to be drawn.
#'
#' @param X          a vector or three-column matrix representing a set of geometric vectors; if a matrix, one vector is drawn for each row
#' @param origin     the origin from which they are drawn, a vector of length 3.
#' @param headlength the \code{headlength} argument passed to \code{\link{arrows3d}} determining the length of arrow heads
#' @param ref.length vector length to be used in scalling arrow heads so that they are all the same size; if \code{NULL}
#'        the longest vector is used to scale the arrow heads
#' @param radius     radius of the base of the arrow heads
#' @param labels     a logical or a character vector of labels for the vectors. If \code{TRUE} and \code{X} is a matrix,
#'        labels are taken from \code{rownames(X)}. If \code{NULL}, no labels are drawn.
#' @param cex.lab    character expansion applied to vector labels. May be a number or numeric vector corresponding to the the
#'        rows of \code{X}, recycled as necessary.
#' @param adj.lab    label position relative to the label point as in \code{\link[rgl]{text3d}}, recycled as necessary.
#' @param frac.lab   location of label point, as a fraction of the distance between \code{origin} and \code{X}, recycled as necessary.
#'        Values \code{frac.lab > 1} locate the label beyond the end of the vector.
#' @param draw       if \code{TRUE} (the default), draw the vector(s).
#' @param ...        other arguments passed on to graphics functions.
#'
#' @return invisibly returns the vector \code{ref.length} used to scale arrow heads
#' @export
#' @author Michael Friendly
#' @seealso \code{\link{arrows3d}}, code{\link[rgl]{texts3d}}, code{\link[rgl]{rgl.material}}
#' @family vector diagrams
#' @import rgl
#'
#' @examples
#' vec <- rbind(diag(3), c(1,1,1))
#' rownames(vec) <- c("X", "Y", "Z", "J")
#' library(rgl)
#' open3d()
#' vectors3d(vec, color=c(rep("black",3), "red"), lwd=2)
#' # draw the XZ plane, whose equation is Y=0
#' planes3d(0, 0, 1, 0, col="gray", alpha=0.2)
#' vectors3d(c(1,1,0), col="green", lwd=2)
#' # show projections of the unit vector J
#' segments3d(rbind(c(1,1,1), c(1, 1, 0)))
#' segments3d(rbind(c(0,0,0), c(1, 1, 0)))
#' segments3d(rbind(c(1,0,0), c(1, 1, 0)))
#' segments3d(rbind(c(0,1,0), c(1, 1, 0)))
#' # show some orthogonal vectors
#' p1 <- c(0,0,0)
#' p2 <- c(1,1,0)
#' p3 <- c(1,1,1)
#' p4 <- c(1,0,0)
#' corner(p1, p2, p3, col="red")
#' corner(p1, p4, p2, col="red")
#' corner(p1, p4, p3, col="blue")
#'
#' rgl.bringtotop()

vectors3d <- function(X, origin=c(0,0,0),
                       headlength=0.035, ref.length=NULL, radius=1/60,
                       labels=TRUE, cex.lab=1.2, adj.lab=0.5, frac.lab=1.1, draw=TRUE, ...) {

  if (is.vector(X)) X <- matrix(X, ncol=3)
  n <- nrow(X)
  X <- rbind(matrix(origin, n, 3), X)
  OX <- X[ as.vector(rbind(1:n, n+1:n)), ]

  scale <- c(1, 1, 1)
#  radius <- 1/60
  ref.length <- arrows3d(OX, headlength=headlength, scale=scale, radius=radius,
                         ref.length=ref.length, draw=draw, ...)
  
  if (draw){
    if (is.logical(labels) && labels) {
      labels <- rownames(X)
    }
    if (!is.null(labels)) {
      # DONE: allow for labels to be positioned some fraction of the way from origin to X
      # FIXME: it is dangerous to use ... for both arrows3d() and text3d(), e.g., for col=
      xl = origin[1] + frac.lab * (X[,1]-origin[1])
      yl = origin[2] + frac.lab * (X[,2]-origin[2])
      zl = origin[3] + frac.lab * (X[,3]-origin[3])
      text3d(xl, yl, zl, labels, cex=cex.lab, adj=adj.lab, ...)
    }
  }
  invisible(c(ref.length=ref.length))
}

