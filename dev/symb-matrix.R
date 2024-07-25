#' Create a symbolic matrix for latex
#'
#' Constructs the latex code for a symbolic matrix, like:
#' \begin{pmatrix}
#'  x_{11}  & x_{12}  & \\dots  & x_{1m}  \\
#'  x_{21}  & x_{22}  & \\dots  & x_{2m}  \\
#'  \\vdots & \\vdots & \\ddots & \\vdots \\
#'  x_{n1}  & x_{n2}  & \\dots  & x_{nm}
#'  \end{pmatrix}
#'
#' @details
#' This so far assumes that the \code{amsmath} package will be available.
#'
#'
#' @param symbol A single character, the name of the matrix elements
#' @param rows   Number of rows.
#' @param cols   Number of columns
#' @param brackets Type of brackets: \code{"p"} uses parentheses "(", ")";  \code{"b"} uses square braqckets "[", "]"; ...
#'
#' @examples
#' symb_matrix("x", rows = "n", cols = "m", brackets = "p)  # default
#' symb_matrix("\\beta", "p", "q")
#'

symb_matrix <- function(
    symbol = "x",
    rows = "n",
    cols = "m",
    brackets=c("p", "b", "B", "V")) {

  brackets <- match.arg(brackets)
  begin <- paste0("\\begin{", brackets, "matrix}")
  end   <- paste0("\\end{", brackets, "matrix}")

  if (is.character(rows)) {}

}
