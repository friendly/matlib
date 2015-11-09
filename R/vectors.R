#' Title Draw Geometric Vectors
#'
#' This function draws vectors in a 2D plot, in a way that facilitates constructing vector diagrams.
#'
#' @param X a vector or two-column matrix representing a set of geometric vectors; if a matrix, one vector is drawn for each row
#' @param origin the origin from which they are drawn, a vector of length 2.
#' @param lwd line width(s) for the vectors, a constant or vector of length equal to the number of rows of \code{X}.
#' @param angle the \code{angle} argument passed to \code{\link[graphics]{arrows}} determining the angle of arrow heads.
#' @param length the \code{length} argument passed to \code{\link[graphics]{arrows}} determining the length of arrow heads.
#' @param labels a logical or a character vector of labels for the vectors. If \code{TRUE} and \code{X} is a matrix,
#'        labels are taken from \code{rownames(X)}. If \code{NULL}, no labels are drawn.
#' @param cex.lab character expansion applied to vector labels
#' @param pos.lab label position relative to the label point
#' @param ... other arguments passed on to graphics functions
#'
#' @return none
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
#' vectors(rbind(u,v,`u+v`=sum), col=c("red", "blue", "purple"))
#' # show the opposing sides of the parallelogram
#' vectors(sum, origin=u, col="red", lty=2)
#' vectors(sum, origin=v, col="blue", lty=2)
#'
#' # projection of vectors
#' vectors(Proj(v,u), labels="P(v,u)", lwd=3)
#' vectors(v, origin=Proj(v,u))

vectors <- function(X, origin=c(0,0),
                    lwd=2, angle=10, length=0.15,
                    labels=TRUE, cex.lab=1.5, pos.lab=4,  ...) {

  if (is.vector(X)) X <- matrix(X, ncol=2)
  arrows(origin[1], origin[2], X[,1], X[,2], lwd=lwd, angle=angle, length=length, ...)
  if (is.logical(labels) && labels) {
    labels <- rownames(X)
  }
  if (!is.null(labels)) {
    # TODO: allow for labels to be positioned some fraction of the way from origin to X
    # FIXME: it is dangerous to use ... for both arrows() and text(), e.g., for col=
    text(X[,1], X[,2], labels, cex=cex.lab, pos=pos.lab, ...)
  }
}
