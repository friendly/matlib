
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
#' @param ref.length vector length to be used in scaling arrow heads so that they are all the same size; if \code{NULL}
#'        the longest vector is used to scale the arrow heads
#' @param radius     radius of the base of the arrow heads
#' @param labels     a logical or a character vector of labels for the vectors. If \code{TRUE} and \code{X} is a matrix,
#'        labels are taken from \code{rownames(X)}. If \code{FALSE} or \code{NULL}, no labels are drawn.
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
#' @seealso \code{\link{arrows3d}}, \code{\link[rgl]{texts3d}}, \code{\link[rgl]{rgl.material}}
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
#'
#' ####
#' # draw tick marks
#'
#' vec0 <- diag(3)
#' vec <- c(1,1,1)
#' rownames(vec0) <- c("X", "Y", "Z")
#' library(rgl)
#' open3d()
#' vectors3d(vec0, color="black", lwd=2, ticks=TRUE)
#' vectors3d(vec, color="red", lwd=2, label='J')
#'

vectors3d <- function(X, origin=c(0,0,0),
                      headlength=0.035, ref.length=NULL, radius=1/60,
                      labels=TRUE, cex.lab=1.2, adj.lab=0.5, frac.lab=1.1,
                      ticks = FALSE, draw=TRUE, ...) {

  if (is.vector(X)) X <- matrix(X, ncol=3)
  n <- nrow(X)
  X <- rbind(matrix(origin, n, 3), X)
  OX <- X[ as.vector(rbind(1:n, n+1:n)), ]

  scale <- c(1, 1, 1)
#  radius <- 1/60
  ref.length <- arrows3d(OX, headlength=headlength, scale=scale, radius=radius,
                         ref.length=ref.length, draw=draw, ...)
  if(ticks){
      for(i in seq(1, nrow(OX), by=2))
        vector_ticks(OX[i,], OX[i+1,], ...)
  }

  if (draw){
    if (is.logical(labels)) {
      if (labels) labels <- rownames(X)
      else labels <- NULL
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


#' @param p0 base of vector
#' @param p1 end of vector
#' @param n_ticks number of ticks
#' @param tick_length length of ticks
#' @param color colour
#' @param label logical; include numeric recording of length?
vector_ticks <- function(p1, p2, n_ticks = 5,
                         tick_length = 0.05, color = "black", labels = FALSE, ...) {
    # 1. Main direction vector and segments
    v <- p2 - p1
    v_len <- sqrt(sum(v^2))
    v_unit <- v / v_len

    # 2. Find a perpendicular vector for the tick direction
    # If v is parallel to Z, cross with X; otherwise cross with Z
    if (abs(v_unit[3]) > 0.9) {
        perp <- c(1, 0, 0)
    } else {
        perp <- c(0, 0, 1)
    }
    tick_dir <- xprod(v_unit, perp)
    tick_dir <- tick_dir / sqrt(sum(tick_dir^2)) # normalize

    # 3. Calculate interpolation points along the line
    t_vals <- seq(0, 1, length.out = n_ticks)
    t_vals <- t_vals[-length(t_vals)]

    tick_starts <- matrix(NA, nrow = n_ticks, ncol = 3)
    tick_ends   <- matrix(NA, nrow = n_ticks, ncol = 3)

    for(i in 1:n_ticks) {
        # Point on the line
        pt_on_line <- p1 + t_vals[i] * v

        # Tick segment coordinates (centered or jutting out to one side)
        tick_starts[i, ] <- pt_on_line - (tick_dir * (tick_length / 2))
        tick_ends[i, ]   <- pt_on_line + (tick_dir * (tick_length / 2))

        # 4. Optional Text Labels

        # Shift text slightly past the end of the tick mark
        text_pos <- pt_on_line + (tick_dir * (tick_length * 1.5))
        if (labels) val <- round(t_vals[i] * v_len, 2) else val <- "" # Labeling by distance from p1
        text3d(text_pos[1], text_pos[2], text_pos[3], texts = val, color = color, cex = 0.8)
    }

    # 5. Interleave start and end points for segments3d
    segments_matrix <- matrix(t(cbind(tick_starts, tick_ends)), ncol = 3, byrow = TRUE)
    segments3d(segments_matrix, color = color, lwd = 1.5)
}

