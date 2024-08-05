#' Create a LaTeX equation wrapper
#'
#' @param expr expression containing \code{\link{cat}}s
#' @param ... expressions using \code{\link{cat}} to be wrapped in a LaTeX math
#'   environment
#' @param number logical; include equation number?
#' @param label character vector specifying the LaTeX label to use (e.g., \code{eqn:myeqn})
#' @param align logical; use the \code{align}
#' @export
#' @examples
#' Eqn(cat('e=mc^2'))
#' Eqn(cat('e=mc^2'), number = FALSE)
#' Eqn(cat('e=mc^2'), label = 'eqn:einstein')
#'
#' Eqn(cat("X=U \\lambda V"), label='eqn:svd')
#'
#' # expressions that use cat() within their calls
#' Eqn({
#'     cat("X=U \\lambda V"), label='eqn:svd')
#'     symbolicMatrix("u", "n", "k", lhs = 'SVD')
#'     symbolicMatrix("\\lambda", "k", "k", diag=TRUE)
#'     symbolicMatrix("v", "k", "p", transpose = TRUE)
#' })
#'
#' # align equations at =
#' Eqn({
#'   cat("X&=U \\lambda V \\\\ \n")
#'   symbolicMatrix("u", "n", "k", lhs = '&')
#'   symbolicMatrix("\\lambda", "k", "k", diag=TRUE)
#'   symbolicMatrix("v", "k", "p", transpose = TRUE)
#' }, align=TRUE)
#'
#' A <- matrix(c(2, 1, -1,
#'               -3, -1, 2,
#'               -2,  1, 2), 3, 3, byrow=TRUE)
#' b <- c(8, -11, -3)
#'
#' matrix2latex(cbind(A,b)) |> Eqn()
#'
Eqn <- function(expr, number = TRUE, label = NULL, align = FALSE) {
  wrap <- if(align) "align" else "equation"
  if(number) wrap <- paste0(wrap, '*')
  cat(sprintf("\n\\begin{%s}\n", wrap))
  if(!is.null(label))
    cat(sprintf('\\label{%s}\n', label))
  eval(expr)
  cat(sprintf("\\end{%s}\n", wrap))
}
