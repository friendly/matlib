##################################
#' Gram-Schmidt Orthogonalization of a Matrix
#'
#' Calculates a matrix with uncorrelated columns using the Gram-Schmidt process
#'
#' This function, originally from the \pkg{heplots} package has now been deprecated in \pkg{matlib}. Use
#' \code{\link{GramSchmidt}} instead.
#'
#' @param y a numeric matrix or data frame
#' @param order if specified, a permutation of the column indices of \code{y}
#' @param recenter logical; if \code{TRUE}, the result has same means as the original \code{y}, else means = 0 for cols 2:p
#' @param rescale  logical; if \code{TRUE}, the result has same sd as original, else, sd = residual sd
#' @param adjnames logical; if \code{TRUE}, colnames are adjusted to Y1, Y2.1, Y3.12, ...
#' @return a matrix/data frame with uncorrelated columns
#' @export
#' @examples
#' \donttest{
#'  set.seed(1234)
#'  A <- matrix(c(1:60 + rnorm(60)), 20, 3)
#'  cor(A)
#'  G <- gsorth(A)
#'  zapsmall(cor(G))
#'  }

# Return a matrix/data frame with uncorrelated columns
#   recenter=TRUE -> result has same means as original, else means = 0 for cols 2:p
#   rescale=TRUE -> result has same sd as original, else, sd = residual sd
#   adjnames=TRUE -> colnames are adjusted to Y1, Y2.1, Y3.12, ...
#  12-5-2010:  Fixed buglet when matrix has no row/col names
#  10-14-2011: Made sd() a local function to avoid deprecated warnings

gsorth <- function(y, order, recenter=TRUE, rescale=TRUE, adjnames=TRUE) {

  # local function sd(), since sd(<matrix>) and sd(<data.frame>) now deprecated
  sd <- function (x, na.rm = FALSE)
  {
    if (is.matrix(x))
      apply(x, 2, sd, na.rm = na.rm)
    else if (is.vector(x))
      sqrt(var(x, na.rm = na.rm))
    else if (is.data.frame(x))
      sapply(x, sd, na.rm = na.rm)
    else sqrt(var(as.vector(x), na.rm = na.rm))
  }

  .Deprecated("GramSchmidt")
  n <- nrow(y)
  if (missing(order)) order <- 1:ncol(y)
  y <- y[,order]
  p <- ncol(y)

  if (is.data.frame(y)) {
    numeric <- unlist(lapply(y, is.numeric))
    if (!all(numeric)) stop("all columns of y must be numeric")
  }

  ybar <- colMeans(y)
  ysd <- sd(y)
  z <- scale(y, center=TRUE, scale=FALSE)
  z <- qr.Q(qr(z))
  zsd <- sd(z)
  if (rescale) z <- z %*% diag( ysd/zsd )
  if (recenter) z <- z + matrix(rep(ybar,times=n), ncol=p, byrow=TRUE)
  rownames(z) <- rownames(y, do.NULL=FALSE)
  colnames(z) <- colnames(y, do.NULL=FALSE)
  if (adjnames) {
    for (j in 2:p) {
      colnames(z)[j] <- paste(colnames(z)[j], '.', sep="",
                              paste( 1:(j-1), collapse=""))
    }
  }
  z
}

