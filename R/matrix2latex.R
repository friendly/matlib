#' (Deprecated) Convert matrix to LaTeX equation
#'
#' (This function has been deprecated; see \code{\link{symbolicMatrix}} instead).
#' This function provides a soft-wrapper to \code{xtable::xtableMatharray()} with additional support for
#' \code{fractions} output and \code{brackets}.
#'
#' @details
#' The code for \code{brackets} matches some of the options from the AMS matrix LaTeX package:
#' \code{\\pmatrix{}}, \code{\\bmatrix{}}, \code{\\Bmatrix{}}, ... .
#'
#'
#' @param x a numeric or character matrix. If the latter a numeric-based arguments will
#'   be ignored
#' @param fractions logical; if \code{TRUE}, try to express non-integers as rational numbers, using the \code{\link[MASS]{fractions}}
#'    function; if you require greater accuracy, you can set the \code{cycles} (default 10)
#'    and/or \code{max.denominator} (default 2000) arguments to \code{fractions} as a global option, e.g.,
#'    \code{options(fractions=list(cycles=100, max.denominator=10^4))}.
#' @param brackets logical or a character in \code{"p", "b", "B", "V"}. If \code{TRUE}, uses square brackets
#'    around the matrix, \code{FALSE} produces no brackets. Otherwise
#'    \code{"p")} uses parentheses, \code{( )};
#'    \code{"b")} uses square brackets \code{[ ]},
#'    \code{"B")} uses braces \code{{ }},
#'    \code{"V")} uses vertical bars \code{| |}.
#' @param show.size logical; if \code{TRUE} shows the size of the matrix as an appended subscript.
#' @param digits Number of digits to display. If \code{digits == NULL} (the default), the function sets
#'     \code{digits = 0} if the elements of \code{x} are all integers
#' @param print  logical; print the LaTeX code for the matrix on the console?; default: \code{TRUE}
#'
#' @param ... additional arguments passed to \code{xtable::xtableMatharray()}
#' @importFrom xtable xtableMatharray
#' @importFrom dplyr case_when
#' @importFrom utils capture.output
#' @author Phil Chalmers
#' @export
#' @examples
#' A <- matrix(c(2, 1, -1,
#'              -3, -1, 2,
#'              -2,  1, 2), 3, 3, byrow=TRUE)
#' b <- c(8, -11, -3)
#'
#' matrix2latex(cbind(A,b))
#' matrix2latex(cbind(A,b), digits = 0)
#' matrix2latex(cbind(A/2,b), fractions = TRUE)
#'
#' matrix2latex(A, digits=0, brackets="p", show.size = TRUE)
#'
#' # character matrices
#' A <- matrix(paste0('a_', 1:9), 3, 3)
#' matrix2latex(cbind(A,b))
#' b <- paste0("\\beta_", 1:3)
#' matrix2latex(cbind(A,b))
#'
matrix2latex <- function(x,
                         fractions = FALSE,
                         brackets = TRUE,
                         show.size = FALSE,
                         digits = NULL,
                         print = TRUE,
                         ...){
  .Deprecated("matrix2latex",
              msg="Function is deprecated. See symbolicMatrix() and Eqn() for more recent approaches")
  if( is.numeric(x) && is.null(digits) && all(trunc(x) == x) ) digits <- 0
  ret <- if (fractions) xtable::xtableMatharray(as.character(Fractions(x)), digits=digits, ...)
         else           xtable::xtableMatharray(x, digits=digits, ...)
  if (is.logical(brackets)) {
    brack = if (isTRUE(brackets)) c("[", "]") else NULL
  }
  else if (length(brackets)==1L)
    brack <- dplyr::case_when(
      brackets == "p" ~ c("(", ")"),
      brackets == "b" ~ c("[", "]"),
      brackets == "B" ~ c("\\{", "\\}"),
      brackets == "V" ~ c("|", "|"),
      .default = ""
    )
#           brackets %in% c("b", "p", "", "B", "v", "V")) brack = "]"

  begin <- end <- NULL
  if (!is.null(brack)) {
    begin <- paste0("\\left", brack[1], "\n")
    end   <- paste0("\\right", brack[2], "\n")
  }
  size <- if (show.size) paste0("_{", nrow(x), " \\times ", ncol(x), "}")
  else NULL

  output <- paste0(capture.output(
      print(ret, sanitize.text.function = function(x){x})), collapse='\n')
  ret <- c(begin, output, end, size)

  if (print) {
      cat(ret)
      return(invisible(NULL))
  } else {
      return(ret)
  }
}
