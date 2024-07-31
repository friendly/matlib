# TODO: fix bug generating extra \\ on the last row when rows is symbolic

#' Create a Symbolic Matrix for LaTeX
#'
#' @description
#' Constructs the latex code for a symbolic matrix, like:
#' \preformatted{
#'  \\begin{pmatrix}
#'    x_{11}  & x_{12}  & \\dots  & x_{1m}  \\
#'    x_{21}  & x_{22}  & \\dots  & x_{2m}  \\
#'    \\vdots & \\vdots & \\ddots & \\vdots \\
#'    x_{n1}  & x_{n2}  & \\dots  & x_{nm}
#'  \\end{pmatrix}
#'  }
#'
#' Alternatively, the number of rows and/or columns can be integers, generating a matrix of given size.
#'
#' As presently designed, the function outputs the LaTeX code to the console, which can then be copied/pasted into a document.
#'
#' @details
#' This implementation assumes that the \code{amsmath} package will be available because it uses the shorthands
#' \code{\\begin{pmatrix}}, ... rather than \code{\\left( ... \\right)}
#'
#'
#' @param symbol A single character, the name of the matrix elements
#' @param rows   Number of rows, a single character representing rows symbolically, or an integer, generating
#'               that many rows.
#' @param cols   Number of columns, a single character representing rows symbolically, or an integer, generating
#'               that many columns.
#' @param brackets Type of brackets: \code{"p"} uses parentheses \code{"(", ")"};
#'               \code{"b"} uses square brackets \code{"[", "]"};
#'               \code{"B"} uses braces \code{"{", "}"};
#'               \code{"v"} uses vertical bars \code{"|", "|"};
#'               \code{"V"} uses double vertical bars \code{"||", "||"};
#'               \code{""} generates a plain matrix without delimeters
#' @param indent characters to indent each line [not yet implemented]
#' @param start base value for indexing rows and columns, \code{0} or \code{1}. The default, \code{start=1} generates
#'              rows indices from 1 to \code{rows}. \code{start=0} generates
#'              rows indices from 0 to \code{rows-1}.
#'
#' @author Michael Friendly
#' @export
#' @examples
#' symb_matrix("x", rows = "n", cols = "m", brackets = "p")  # default
#' symb_matrix("\\beta", "p", "q")
#' symb_matrix("\\beta", "p", "q", start=0)
#'
#' # numeric rows/cols
#' symb_matrix("y", "p", 5)
#' symb_matrix("y", 4, "q")
#' symb_matrix("y", 4, 4)
#' symb_matrix("\\beta", 4, 4, start=0)

symb_matrix <- function(
    symbol = "x",
    rows = "n",
    cols = "m",
    brackets=c("p", "b", "B", "v", "V", ""),
    indent = "\t",
    start = 1) {

  brackets <- match.arg(brackets)
  begin <- paste0("\\begin{", brackets, "matrix}\n\t")
  end   <- paste0("\\end{", brackets, "matrix}\n")

  # make a symbolic row
  symb_row <- function(symbol, i, cols) {
    ind <- if(start==1) 1:3 else 0:3
    row <- paste0(symbol, "_{", i, ind, "}")
    row[3] <- "\\dots"
    row[4] <- paste0(symbol, "_{", i, cols, "}")
    row
  }

  # make a numeric row
  numb_row <- function(symbol, i, cols) {
    ind <- if(start==1) 1:cols else 0:(cols-1)
    row <- paste0(symbol, "_{", i, ind, "}")
    row
  }

  if (is.character(rows)) {   # rows is symbolic
    if (is.character(cols)) {
#      ind <- if(start==1) 1:cols else 0:(cols-1)
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
    if (is.character(cols)) {   # cols is symbolic
      ind <- if(start==1) 1:rows else 0:(rows-1)
      mat <- rep("", rows)
      for(i in seq(rows)) {
        mat[i] <- symb_row(symbol, ind[i], cols) |> paste(collapse = " & ")
      }
    }
    else {    # cols is numeric
      ind <- if(start==1) 1:rows else 0:(rows-1)
      mat <- rep("", rows)
      for(i in seq(rows)) {
        mat[i] <- numb_row(symbol, ind[i], cols) |> paste(collapse = " & ")
      }
    }
  }

  # end each with \\
  # should indent lines
  result <- paste(mat, collapse = " \\\\ \n\t")
#  result <- c(begin, result, "\n", end)
  result <- c(begin, result, end)
  cat(result)
}
