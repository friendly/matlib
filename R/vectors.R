#' Draw geometric vectors in 2D
#'
#' This function draws vectors in a 2D plot, in a way that facilitates constructing vector diagrams. It allows vectors to be
#' specified as rows of a matrix, and can draw labels on the vectors.
#'
#' @param X a vector or two-column matrix representing a set of geometric vectors; if a matrix, one vector is drawn for each row
#' @param origin the origin from which they are drawn, a vector of length 2.
#' @param lwd line width(s) for the vectors, a constant or vector of length equal to the number of rows of \code{X}.
#' @param angle the \code{angle} argument passed to \code{\link[graphics]{arrows}} determining the angle of arrow heads.
#' @param length the \code{length} argument passed to \code{\link[graphics]{arrows}} determining the length of arrow heads.
#' @param labels a logical or a character vector of labels for the vectors. If \code{TRUE} and \code{X} is a matrix,
#'        labels are taken from \code{rownames(X)}. If \code{NULL}, no labels are drawn.
#' @param cex.lab character expansion applied to vector labels. May be a number or numeric vector corresponding to the the
#'        rows of \code{X}, recycled as necessary.
#' @param pos.lab label position relative to the label point as in \code{\link[graphics]{text}}, recycled as necessary.
#' @param frac.lab location of label point, as a fraction of the distance between \code{origin} and \code{X}, recycled as necessary.
#'        Values \code{frac.lab > 1} locate the label beyond the end of the vector.
#' @param ... other arguments passed on to graphics functions.
#'
#' @return none
#' @seealso \code{\link[graphics]{arrows}}, \code{\link[graphics]{text}}
#' @family vector diagrams
#' @export
#' @importFrom graphics arrows
#'
#' @examples
#' # shows addition of vectors
#' u <- c(3,1)
#' v <- c(1,3)
#' sum <- u+v
#'
#' xlim <- c(0,5)
#' ylim <- c(0,5)
#' # proper geometry requires asp=1
#' plot( xlim, ylim, type="n", xlab="X", ylab="Y", asp=1)
#' abline(v=0, h=0, col="gray")
#'
#' vectors(rbind(u,v,`u+v`=sum), col=c("red", "blue", "purple"), cex.lab=c(2, 2, 2.2))
#' # show the opposing sides of the parallelogram
#' vectors(sum, origin=u, col="red", lty=2)
#' vectors(sum, origin=v, col="blue", lty=2)
#'
#' # projection of vectors
#' vectors(Proj(v,u), labels="P(v,u)", lwd=3)
#' vectors(v, origin=Proj(v,u))
#' corner(c(0,0), Proj(v,u), v, col="grey")

vectors <- function(X, origin=c(0,0),
                    lwd=2, angle=13, length=0.15,
                    labels=TRUE, cex.lab=1.5, pos.lab=4, frac.lab=1,  ...) {

  if (is.vector(X)) X <- matrix(X, ncol=2)
  .arrows(origin[1], origin[2], X[,1], X[,2], lwd=lwd, length=length, angle=angle, ...)
  if (is.logical(labels) && labels) {
    labels <- rownames(X)
  }
  if (!is.null(labels)) {
    # DONE: allow for labels to be positioned some fraction of the way from origin to X
    # FIXME: it is dangerous to use ... for both arrows() and text(), e.g., for col=
    xl = origin[1] + frac.lab * (X[,1]-origin[1])
    yl = origin[2] + frac.lab * (X[,2]-origin[2])
    text(xl, yl, labels, cex=cex.lab, pos=pos.lab, ...)
  }
}

# the following function isn't exported

.arrows <- function(..., angle=13){
  angles <- seq(1, angle, by=2)
  for (ang in angles) arrows(..., angle=ang)
}
