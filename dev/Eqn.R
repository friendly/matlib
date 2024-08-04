#' Create a LaTeX equation
#'
#' @param ...
#' @param number
#' @param label character vector specifying the LaTeX label to use (e.g., \code{eqn:myeqn})
#' @examples
#' Eqn(cat('y=mc^2'))
#' Eqn(cat('y=mc^2'), number = FALSE)
#' Eqn(cat('y=mc^2'), label = 'eqn:einstein')
#'
#' Eqn(cat("X=U \\lambda V"), label='eqn:svd')


Eqn <- function(..., number = TRUE, label = NULL) {
  wrap <- if(number) "equation" else "equation*"
  cat(sprintf("\n\\begin{%s}\n", wrap))
  if(!is.null(label))
    cat(sprintf('\\label{%s}\n', label))
  eval(...)
  cat(sprintf("\n\\end{%s}\n", wrap))
}
