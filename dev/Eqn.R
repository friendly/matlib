#' Create a LaTeX equation wrapper
#'
#' @param expr expression containing \code{\link{cat}}s
#' @param ... expressions using \code{\link{cat}} to be wrapped in a LaTeX math
#'   environment
#' @param number logical; include equation number?
#' @param label character vector specifying the LaTeX label to use (e.g., \code{eqn:myeqn})
#' @export
#' @examples
#' Eqn(cat('e=mc^2'))   # equivalent
#' Eqn(cat('e=mc^2'), number = FALSE)
#' Eqn(cat('e=mc^2'), label = 'eqn:einstein')
#'
#' Eqn(cat("X=U \\lambda V"), label='eqn:svd')
#'
#' # expressions that use cat() within their calls
#' Eqn({
#'     symbolicMatrix("u", "n", "k", lhs = 'SVD')
#'     symbolicMatrix("\\lambda", "k", "k", diag=TRUE)
#'     symbolicMatrix("v", "k", "p", transpose = TRUE)
#' })
#'
Eqn <- function(expr, number = TRUE, label = NULL) {
  wrap <- if(number) "equation" else "equation*"
  cat(sprintf("\n\\begin{%s}\n", wrap))
  if(!is.null(label))
    cat(sprintf('\\label{%s}\n', label))
  eval(expr)
  cat(sprintf("\\end{%s}\n", wrap))
}
