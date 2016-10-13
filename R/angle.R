#
#' Angle between two vectors
#'
#' \code{angle} calculates the angle between two vectors.
#'
#' @param x a numeric vector
#' @param y a numeric vector
#' @param degree logical; should the angle be computed in degrees? 
#'   If \code{FALSE} the result is returned in radians
#'
#' @return a scalar containing the angle between the vectors
#' @seealso \code{\link{len}}
#' @export
#' @examples
#' x <- c(2,1)
#' y <- c(1,1)
#' angle(x, y) # degrees
#' angle(x, y, degree = FALSE) # radians
#' 
#' # visually
#' xlim <- c(0,2.5)
#' ylim <- c(0,2)
#' # proper geometry requires asp=1
#' plot( xlim, ylim, type="n", xlab="X", ylab="Y", asp=1,
#'   main = expression(theta == 18.4))
#' abline(v=0, h=0, col="gray")
#' vectors(rbind(x,y), col=c("red", "blue"), cex.lab=c(2, 2)) 
#' text(.5, .37, expression(theta))
#' 
#' 
#' ####
#' x <- c(-2,1)
#' y <- c(1,1)
#' angle(x, y) # degrees
#' angle(x, y, degree = FALSE) # radians
#' 
#' # visually
#' xlim <- c(-2,1.5)
#' ylim <- c(0,2)
#' # proper geometry requires asp=1
#' plot( xlim, ylim, type="n", xlab="X", ylab="Y", asp=1,
#'   main = expression(theta == 108.4))
#' abline(v=0, h=0, col="gray")
#' vectors(rbind(x,y), col=c("red", "blue"), cex.lab=c(2, 2)) 
#' text(0, .4, expression(theta), cex=1.5)

angle <- function(x, y, degree = TRUE) {
  theta <- acos(x %*% y / (len(x) * len(y)))
  if(degree) theta <- r2d(theta)
  theta
}
