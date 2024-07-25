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
#' The intention is to allow rows/cols to be either a character or an integer
#'
#'
#' @param symbol A single character, the name of the matrix elements
#' @param rows   Number of rows, a single character representing rows symbolically, or an integer, generating that many rows.
#' @param cols   Number of columns
#' @param brackets Type of brackets: \code{"p"} uses parentheses "(", ")";  \code{"b"} uses square braqckets "[", "]"; ...
#'
#' @examples
#' symb_matrix("x", rows = "n", cols = "m", brackets = "p)  # default
#' symb_matrix("\\beta", "p", "q")
#'
#' # numeric rows/cols
#' symb_matrix("y", "p", 5)

symb_matrix <- function(
    symbol = "x",
    rows = "n",
    cols = "m",
    brackets=c("p", "b", "B", "V")) {

  brackets <- match.arg(brackets)
  begin <- paste0("\\begin{", brackets, "matrix}\n\t")
  end   <- paste0("\\end{", brackets, "matrix}\n")

  # make a symbolic row
  symb_row <- function(symbol, i, cols) {
    row <- paste0(symbol, "_{", i, 1:4, "}")
    row[3] <- "\\dots"
    row[4] <- paste0(symbol, "_{", i, cols, "}")
    row
  }

  # make a numeric row
  numb_row <- function(symbol, i, cols) {
    row <- paste0(symbol, "_{", i, 1:cols, "}")
    row
  }

  if (is.character(rows)) {
    if (is.character(cols)) {
      mat <- rep("", 4)
      mat[1] <- symb_row(symbol, 1, cols) |> paste(collapse = " & ")
      mat[2] <- symb_row(symbol, 2, cols) |> paste(collapse = " & ")
      mat[3] <- c("\\vdots", "\\vdots", "\\ddots", "\\vdots") |> paste(collapse = " & ")
      mat[4] <- symb_row(symbol, rows, cols) |> paste(collapse = " & ")
    }
    else {    # cols is numeric
      mat <- rep("", cols)
      mat[1] <- numb_row(symbol, 1, cols) |> paste(collapse = " & ")
      mat[2] <- numb_row(symbol, 2, cols) |> paste(collapse = " & ")
      mat[3] <- rep("\\vdots", cols) |> paste(collapse = " & ")
      mat[4] <- numb_row(symbol, rows, cols) |> paste(collapse = " & ")
    }
  }
  else {   # rows is numeric
    if (is.character(cols)) {
    }
  }

  # end each with \\
  # should indent lines
  result <- paste(mat, collapse = " \\\\ \n\t")
<<<<<<< HEAD
#  result <- c(begin, result, "\n", end)
  result <- c(begin, result, end)
=======
  result <- c(begin, result, "\n", end)
>>>>>>> 3b2e99ed7bcc9a97313ae3a86d9c125002aed6eb
  cat(result)
}
