#' Rank of a Matrix
#'
#' Returns the rank of a matrix \code{X}, using the QR decomposition, \code{\link{QR}}.
#' Included here as a simple function, because \code{rank} does something different
#' and it is not obvious what to use for matrix rank.
#'
#' @param X a matrix
#' @return rank of \code{X}
#' @export
#' @seealso \code{\link[base]{qr}}
#'
#' @examples
#' M <- outer(1:3, 3:1)
#' M
#' R(M)
#'
#' M <- matrix(1:9, 3, 3)
#' M
#' R(M)
#' # why rank=2?
#' echelon(M)
#'
#' set.seed(1234)
#' M <- matrix(sample(1:9), 3, 3)
#' M
#' R(M)
R <- function(X) {
  QR(X)$rank
}
