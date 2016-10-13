#' Trace of a Matrix
#'
#' Calculates the trace of a square numeric matrix, i.e., the sum of its diagonal elements
#'
#' @param X a numeric matrix
#' @return a numeric value, the sum of \code{diag(X)}
#' @export
#' @examples
#' X <- matrix(1:9, 3, 3)
#' tr(X)
#'
tr <- function(X) {
  if (!is.numeric(X) || !is.matrix(X) || !nrow(X) == ncol(X)) stop("X must be a square numeric matrix")
  sum(diag(X))
}
