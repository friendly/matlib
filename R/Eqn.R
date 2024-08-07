#' Create a LaTeX Equation Wrapper
#'
#' @description
#'
#' This function is designed to produce LaTeX expressions that can be copied/pasted into documents or
#' used directly in \code{.Rmd} or \code{.qmd} documents to compile to equations.
#' It wraps the equations in either a \code{\\begin{equation} ...\\end{equation}} or
#' \code{\\begin{align} ...\\end{align}} environment.
#'
#' In a code chunk, use the chunk options \code{results='asis', echo=FALSE}.
#'
#' @param ... expressions using \code{\link{cat}} and
#'   return a \code{NULL} object to be wrapped in a LaTeX math
#'   environment. Supplying a character vector will automatically wrap the
#'   expression inside a call to \code{\link{cat}}
#' @param number logical; include equation number? Default: {TRUE}
#' @param label character vector specifying the LaTeX label to use (e.g., \code{eqn:myeqn})
#' @param align logical; use the \code{align} environment with explicit \code{&}. Default: {FALSE}
#' @returns NULL
#' @author Phil Chalmers
#' @seealso \code{\link{symbolicMatrix}}, \code{\link{matrix2latex}}
#' @export
#' @examples
#'
#' Eqn('e=mc^2')
#' Eqn(cat('e=mc^2 \n')) # equivalent, but unnecessary
#'
#' # Equation numbers & labels
#' Eqn('e=mc^2', number = FALSE)
#' Eqn('e=mc^2', label = 'eqn:einstein')
#' Eqn("X=U \\lambda V", label='eqn:svd')
#'
#' # Multiple expressions
#' Eqn("e=mc^2", "X=U \\lambda V", label='eqn:svd')
#'
#' # expressions that use cat() within their calls
#' Eqn(symbolicMatrix("u", "n", "k", lhs = 'SVD'),
#'     symbolicMatrix("\\lambda", "k", "k", diag=TRUE),
#'     symbolicMatrix("v", "k", "p", transpose = TRUE),
#'     label='eqn:svd')
#'
#' # align equations using & operator
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
#' # with showEqn()
#' showEqn(A, b, latex=TRUE) |> Eqn()
#'
Eqn <- function(...,
                number = TRUE,
                label = NULL,
                align = FALSE) {
  wrap <- if(align) "align" else "equation"
  if(!number) wrap <- paste0(wrap, '*')
  cat(sprintf("\n\\begin{%s}\n", wrap))
  if(!is.null(label))
    cat(sprintf('\\label{%s}\n', label))
  tmp <- substitute(deparse(...))
  is_char <- sapply(tmp, is.character)[-1L]
  chartmp <- as.character(tmp)[-1L]
  for(i in 1L:length(chartmp)){
      if(is_char[i]){
          if(i > 1L && is_char[i-1L]) cat("\n")
          cat(chartmp[i])
          if(i == length(chartmp)) cat("\n")
      } else eval(parse(text = chartmp[i]))
  }
  cat(sprintf("\\end{%s}\n", wrap))
  invisible(NULL)
}

#' Emit a newline in an equation
#'
#' @rdname Eqn
#' @export
Eqn_newline <- function() cat(' \\\\ \n')
