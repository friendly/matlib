# code taken from pca3d:  objects3d.R



# return the basic cone mesh
# scale is necessary because of the dependence on the aspect ratio
.getcone <- function( r, h, scale= NULL ) {

  ## for drawing circles in 3D, precalculate some values
  .sin.t <- sin(seq(0, 2 * pi, len= 10))
  .cos.t <- cos(seq(0, 2 * pi, len= 10))

  n  <- length( .sin.t )
  xv <- r * .sin.t
  yv <- rep( 0, n )
  zv <- r * .cos.t

  if( missing( scale ) ) scale <- rep( 1, 3 )

  scale <- 1 / scale
  sx <- scale[1]
  sy <- scale[2]
  sz <- scale[3]

  tmp <- NULL
  for( i in 1:(n-1) ) {
    tmp <- rbind( tmp,
      c( 0, 0, 0 ),
      scale3d( c( xv[i],   yv[i],   zv[i]   ), sx, sy, sz ),
      scale3d( c( xv[i+1], yv[i+1], zv[i+1] ), sx, sy, sz ) )
  }
  for( i in 1:(n-1) ) {
    tmp <- rbind( tmp,
      c( 0, h, 0 ),
      scale3d( c( xv[i],   yv[i],   zv[i]   ), sx, sy, sz ),
      scale3d( c( xv[i+1], yv[i+1], zv[i+1] ), sx, sy, sz ) )
  }
  tmp
}

# vector cross product
.cross3 <- function(a,b) {
  c(a[2]*b[3]-a[3]*b[2], -a[1]*b[3]+a[3]*b[1], a[1]*b[2]-a[2]*b[1])
}

#' Draw a 3D cone
#'
#' Draws a cone in 3D from a \code{base} point to a \code{tip} point, with a given \code{radius} at the base.
#' This is used to draw nice arrow heads in \code{\link{arrows3d}}.
#'
#' @param base   coordinates of base of the cone
#' @param tip    coordinates of tip of the cone
#' @param radius radius of the base
#' @param col    color
#' @param scale  scale factor for base and tip
#' @param ...    rgl arguments passed down; see \code{\link[rgl]{rgl.material}}
#'
#' @return       returns the integer object ID of the shape that was added to the scene
#' @author       January Weiner, borrowed from from the \pkg{pca3d} package
#' @export
#' @import rgl
#' @seealso \code{\link{arrows3d}}
#'
#' @examples
#' # none yet

cone3d <- function( base, tip, radius= 10, col= "grey", scale= NULL, ... ) {
#  start <- rep( 0, 3 )

  if( missing( scale ) ) scale <- 1 # was: rep( 1, 0 )
  else scale <- max( scale ) / scale


  tip  <- as.vector( tip ) * scale
  base <- as.vector( base ) * scale

  v1 <- tip
  v2 <- c( 0, 100, 0 )
  o <- .cross3( v1, v2 )
  theta <- acos( sum( v1 * v2 ) / ( sqrt(sum( v1  *  v1 )) * sqrt(sum( v2  *  v2 )) ) )
  vl <- sqrt( sum( tip^2 ) )

  tmp <- .getcone( radius, vl )
  tmp <- translate3d( rotate3d( tmp, theta, o[1], o[2], o[3] ), base[1], base[2], base[3] )
  scale <- 1 / scale
  tmp <- t( apply( tmp, 1, function( x ) x * scale ) )
  triangles3d( tmp, col= col, ... )
}


#' Draw 3D arrows
#'
#' Draws nice 3D arrows with \code{cone3d}s at their tips.
#'
#' This function is meant to be analogous to \code{\link[graphics]{arrows}}, but for 3D plots using \code{\link[rgl]{rgl}}.
#' \code{headlength}, \code{scale} and \code{radius} set the length, scale factor and base radius of the arrow head, a
#' 3D cone. The units of these are all in terms of the ranges of the current rgl 3D scene.
#'
#' @param coords     A 2n x 3 matrix giving the start and end (x,y,z) coordinates of n arrows, in pairs.  The first vector
#'                   in each pair is taken as the starting coordinates of the arrow, the second as the end coordinates.
#' @param headlength Length of the arrow heads, in device units
#' @param head       Position of the arrow head. Only \code{head="end"} is presently implemented.
#' @param scale      Scale factor for base and tip of arrow head, a vector of length 3, giving relative scale factors for X, Y, Z
#' @param radius     radius of the base of the arrow head
#' @param ref.length length of vector to be used to scale all of the arrow heads (permits drawing arrow heads of the same size as in a previous call);
#'                   if \code{NULL}, arrows are scaled relative to the longest vector
#' @param draw       if \code{TRUE} (the default) draw the arrow(s)
#' @param ...        rgl arguments passed down to \code{\link[rgl]{segments3d}} and \code{cone3d}, for example, \code{col} and \code{lwd}
#'
#' @return           invisibly returns the length of the vector used to scale the arrow heads
#' @author           January Weiner, borrowed from the \pkg{pca3d} package, slightly modified by John Fox
#' @seealso          \code{\link{vectors3d}}
#' @family vector diagrams
#' @export
#'
#' @examples
#'  #none yet
arrows3d <- function( coords, headlength= 0.035, head= "end", scale= NULL, radius = NULL,
                      ref.length=NULL, draw=TRUE, ... ) {

  head <- match.arg( head, c( "start", "end", "both" ) )
  # FIXME:  check whether coords is a matrix of 3 cols, and an even # of rows
  narr <- nrow( coords ) / 2
  n    <- nrow( coords )

  starts <- coords[ seq( 1, n, by= 2 ), , drop=FALSE]
  ends   <- coords[ seq( 2, n, by= 2 ), , drop=FALSE]
  if( missing( radius ) ) radius <- ( max( coords ) - min( coords ) ) / 50

  lengths <- sqrt(rowSums(ends - starts)^2)

  if (is.null(ref.length)){
    ref.length <- max(lengths)
  }
  
  if (draw){
    segments3d( coords, ... )
    if( head == "end" | head == "both" ) {
      for( i in 1:narr ) {
        s <- starts[i,]
        e <- ends[i,]
        base <- e - ( e - s ) * headlength * ref.length/lengths[i]
        tip  <- ( e - s ) * headlength * ref.length/lengths[i]
        cone3d( base, tip, radius= radius, scale= scale, ... )
      }
    }
  }
  invisible(c(ref.length=ref.length))
}

.show.axes <- function(axes.color, ranges) {
  axes <- rbind(
    c(ranges[1,1], 0, 0),
    c(ranges[2,1], 0, 0),
    c(0, ranges[1,2], 0),
    c(0, ranges[2,2], 0),
    c(0, 0, ranges[1,3]),
    c(0, 0, ranges[2,3])
   )
  segments3d(axes, col= axes.color)

  radius <- 10

  scale <- c(1, 1, 1)
  if(! missing(ranges)) {
    scale <- ranges[2,]
    radius <- max(scale) / 50
  }

  arrows3d(axes, radius= radius, scale= scale,  col= axes.color)
}

# TESTME <- FALSE
# if (TESTME) {
#   ranges <- rbind(min=c(0,0,0), max=c(1,1,1))
#   open3d()
#   .show.axes(c("red", "green", "blue"), ranges)
# }

