#' Create a LaTeX equation wrapper
#'
#' @param ... expressions using \code{\link{cat}} and
#'   return a \code{NULL} object to be wrapped in a LaTeX math
#'   environment. Supplying a character vector will automatically wrap the
#'   expression inside a call to \code{\link{cat}}
#' @param number logical; include equation number?
#' @param label character vector specifying the LaTeX label to use (e.g., \code{eqn:myeqn})
#' @param align logical; use the \code{align} environment with explicit \code{&}
#'   aligners?
#' @export
#' @examples
#'
#' Eqn('e=mc^2')
#' Eqn(cat('e=mc^2')) # equivalent, but unnecessary
#' Eqn('e=mc^2', number = FALSE)
#' Eqn('e=mc^2', label = 'eqn:einstein')
#' Eqn("X=U \\lambda V", label='eqn:svd')
#'
#' # expressions that use cat() within their calls (excluding first argument)
#' Eqn("X=U \\lambda V",
#'     Eqn_newline(),
#'     symbolicMatrix("u", "n", "k", lhs = 'SVD'),
#'     symbolicMatrix("\\lambda", "k", "k", diag=TRUE),
#'     symbolicMatrix("v", "k", "p", transpose = TRUE),
#'     label='eqn:svd')
#'
#' # align equations at =
#' Eqn("X &= U \\lambda V",
#'     Eqn_newline(),
#'     symbolicMatrix("u", "n", "k", lhs = '&'),
#'     symbolicMatrix("\\lambda", "k", "k", diag=TRUE),
#'     symbolicMatrix("v", "k", "p", transpose = TRUE),
#'     align=TRUE)
#'
#' #  matrix2latex() example
#' A <- matrix(c(2, 1, -1,
#'               -3, -1, 2,
#'               -2,  1, 2), 3, 3, byrow=TRUE)
#' b <- c(8, -11, -3)
#'
#' matrix2latex(cbind(A,b)) |> Eqn()
#'
Eqn <- function(..., number = TRUE, label = NULL, align = FALSE) {
  wrap <- if(align) "align" else "equation"
  if(!number) wrap <- paste0(wrap, '*')
  cat(sprintf("\n\\begin{%s}\n", wrap))
  if(!is.null(label))
    cat(sprintf('\\label{%s}\n', label))
  tmp <- substitute(deparse(...))
  is_char <- sapply(tmp, is.character)[-1L]
  chartmp <- as.character(tmp)[-1L]
  for(i in 1L:length(chartmp)){
      if(is_char[i]) cat(chartmp[i])
      else eval(parse(text = chartmp[i]))
  }
  cat(sprintf("\\end{%s}\n", wrap))
  invisible(NULL)
}

Eqn_newline <- function(form = ' \\\\ \n') cat(form)
