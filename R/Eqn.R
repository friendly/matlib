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
#' @param ... expressions that provide a) a \code{\link{cat}} call and
#'   returns a \code{NULL} object to be wrapped in a LaTeX math
#'   environment, b) a character vector, which will be automatically wrapped the
#'   expression inside a call to \code{\link{cat}}, or c) an object of class
#'   \code{\link{symbolicMatrix}}
#' @param number logical; include equation number? Default: \code{TRUE}
#' @param label character vector specifying the LaTeX label to use (e.g., \code{eqn:myeqn}), which
#'   can be reference via \code{\ref{eqn:myeqn}}. For compiled documents, if an
#'   HTML output is detected (see \code{html_output}) then the equations will be labelled
#'   via \code{(\#eqn:myeqn)} and references via \code{\@ref(eq:binom)}
#' @param html_output logical; use labels for HTML outputs instead of the LaTeX? Automatically
#'   changed for compiled documents that support \code{knitr}
#' @param align logical; use the \code{align} environment with explicit \code{&}. Default: \code{FALSE}
#' @returns NULL
#' @importFrom knitr is_html_output
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
#' # html output (auto detected for documents)
#' Eqn('e=mc^2', label = 'eqn:einstein', html_output = TRUE)
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
                align = FALSE,
                html_output = knitr::is_html_output()) {

  sink.reset <- function(){
      if(sink.number() > 1L){
        for(i in seq_len(sink.number())){
          sink(NULL)
        }
      }
  }

  on.exit(sink.reset())
  wrap <- if(align) "align" else "equation"
  if(!number) wrap <- paste0(wrap, '*')
  cat(sprintf("\n\\begin{%s}\n", wrap))
  if(!is.null(label)){
      if(html_output)
          cat(sprintf('(\\#%s)\n', label))
      else cat(sprintf('\\label{%s}\n', label))
  }
  tmp <- substitute(deparse(...))
  is_char <- sapply(tmp, is.character)[-1L]
  chartmp <- as.character(tmp)[-1L]
  sink(tempfile())
  dots <- list(...)
  sink()
  for(i in 1L:length(chartmp)){
      if(is_char[i]){
          if(i > 1L && is_char[i-1L]) cat("\n")
          cat(chartmp[i])
          if(i == length(chartmp)) cat("\n")
      } else if(is(dots[[i]], 'symbolicMatrix')){
          print(dots[[i]])
      } else {
          eval(parse(text = chartmp[i]))
      }
  }
  cat(sprintf("\\end{%s}\n", wrap))
  invisible(NULL)
}

#' Emit a newline in an equation
#'
#' @rdname Eqn
#' @export
Eqn_newline <- function() cat(' \\\\ \n')
