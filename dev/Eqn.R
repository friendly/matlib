#' Create a LaTeX equation wrapper
#'
#' @param expr first expression to include. If a character vector will be
#'   wrapped in \code{\link{cat}}
#' @param ... expressions using \code{\link{cat}} to be wrapped in a LaTeX math
#'   environment
#' @param number logical; include equation number?
#' @param label character vector specifying the LaTeX label to use (e.g., \code{eqn:myeqn})
#' @export
#' @examples
#' Eqn('e=mc^2')
#' Eqn(cat('e=mc^2'))   # equivalent
#' Eqn('e=mc^2', number = FALSE)
#' Eqn('e=mc^2', label = 'eqn:einstein')
#'
#' Eqn("X=U \\lambda V", label='eqn:svd')
#'
#' # expressions that use cat() within their calls
#' Eqn({
#'     symbolicMatrix("u", "n", "k", lhs = 'SVD')
#'     symbolicMatrix("\\lambda", "k", "k", diag=TRUE)
#'     symbolicMatrix("v", "k", "p", transpose = TRUE)
#' })
#'
Eqn <- function(expr, ..., number = TRUE, label = NULL) {
  dots <- list(...)
  wrap <- if(number) "equation" else "equation*"
  cat(sprintf("\n\\begin{%s}\n", wrap))
  if(!is.null(label))
    cat(sprintf('\\label{%s}\n', label))
  if(is.character(expr)) cat(expr)
  if(length(dots)) eval(...)
  cat(sprintf("\\end{%s}\n", wrap))
}
