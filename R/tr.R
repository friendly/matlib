#' Trace of a Matrix
#'
#' Calculates the trace of a numeric matrix, i.e., the sum of its diagonal elements
#'
#' @param X a numeric matrix
#' @return a numeric value, the sum of \code{diag(X)}
#' @examples
#' X <- matrix(1:9, 3, 3)
#' tr(X)
#'
tr <- function(X) {
  if (!is.numeric(X) || !is.matrix(X)) stop("X must be a numeric matrix")
  sum(diag(X))
}
