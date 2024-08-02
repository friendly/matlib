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
#'#' Alternatively, the number of rows and/or columns can be integers, generating a matrix of given size.
#'
#' @details
#' This implementation assumes that the \code{amsmath} package will be available because it uses the shorthands
#' \code{\\begin{pmatrix}}, ... rather than \code{\\left( \begin{array} ... \end{array} \\right)}
#'
#' @param symbol name for matrix elements, character string. For LaTeX symbols, the backslash must be escaped, e.g, \code{\\beta}.
#' @param nrow   Number of rows, a single character representing rows symbolically, or an integer, generating
#'               that many rows.
#' @param ncol   Number of columns, a single character representing columns symbolically, or an integer, generating
#'               that many columns.
#' @param matrix Character string giving the LaTeX matrix environment used in \code{\\begin{}}, \code{\\end{}}. Typically one of
#'               \code{"pmatrix"} (uses parentheses \code{"(", ")"});
#'               \code{"bmatrix"} (uses square brackets \code{"[", "]"});
#'               \code{"Bmatrix"} (uses braces \code{"{", "}"};),
#'               \code{"vmatrix"} (uses vertical bars \code{"|", "|"});
#'               \code{"Vmatrix"} (uses doublevertical bars \code{"||", "||"});
#'               \code{"matrix"} (generates a plain matrix without delimeters).
#'
#' @param diag   logical; if TRUE, off-diagonal elements are all 0 (and \code{nrow} must == \code{ncol})
#' @param comma  if TRUE, commas are inserted between row and column subscripts
#' @param exponent if specified, e.g., "-1", or "1/2",  the exponent is applied to the matrix
#' @param transpose if TRUE, the transpose symbol "\\top" is appended to the matrix; may
#'               also be a character, e.g., \code{"T"}, \code{"\\prime"}, \code{"\textsf{T}"}
#' @param print  print the LaTeX code for the matrix on the console
#'
#' @returns Returns invisibly the LaTeX representation of the matrix as a character sting.
#'        Use \code{cat()} to display it at the console, or \code{clipr::write_clip()} to copy it to the clipboard
#'
#' @author John Fox
#' @export
symbolicMatrix <- function(
    symbol="x",
    nrow="n",
    ncol="m",
    matrix="pmatrix",
    diag=FALSE,
    comma=FALSE,
    exponent,
    transpose=FALSE,
    print=TRUE){

  # Args:
  #   symbol:
  #   nrow: number of rows, can be a character
  #   ncol: number of columns, can be a character
  #   matrix: LaTeX matix environment
  #   diag: if TRUE, off-diagonal elements are all 0 (and nrow must == ncol)
  #   comma: if TRUE, commas are inserted between row and column subscripts
  #   exponent: if specified, e.g., "-1", or "1/2",  the exponent is applied to the matrix
  #   transpose: if TRUE, the transpose symbol "\\top" is appended to the matrix; may
  #              also be a character, e.g., "T" or "\\prime"
  #   print: print the LaTeX code for the matrix on the console

  if (is.numeric(nrow)){
    if (round(nrow) != nrow || nrow <= 0)
      stop("nrow is not a positive whole number")
  }

  if (is.numeric(ncol)){
    if (round(ncol) != ncol || ncol <= 0)
      stop("ncol is not a positive whole number")
  }

  comma <- if (comma) "," else ""
  if (isTRUE(transpose)) transpose <- "\\top"
  if (!missing(exponent) && !isFALSE(transpose)){
    exponent <- paste0("{", exponent, "^", transpose, "}")
    transpose <- FALSE
  }

  row.elements <- c(symbol, symbol, "\\cdots", symbol)
  col.subscripts <- c("1", "2", "", ncol)
  left.sub <- c("_{", "_{", "", "_{")
  right.sub <- c("}", "}", "", "}")
  post.element <- c(" & ", " & ", " & ", " \\\\ \n")

  result <- paste0("\\begin{", matrix, "} \n")

  if (diag){
    if (nrow != ncol) stop("nrow and ncol must be the same if diag = TRUE")
    if (is.numeric(nrow)){
      mat <- matrix("0", nrow, nrow)
      diag(mat) <- paste0(symbol, "_{", 1:nrow, "}")
    } else {
      mat <- matrix("0", 4, 4)
      mat[3, ] <- "\\vdots"
      mat[, 3] <- "\\cdots"
      mat[3, 3] <- "\\ddots"
      mat[cbind(c(1, 2, 4), c(1, 2, 4))] <- paste0(symbol,
                                                   c("_1", "_2", paste0("_{", nrow, "}")))
    }
    if (is.character(nrow)) nrow <- 4
    for (i in 1:nrow){
      result <- paste0(result, "  ")
      for (j in 1:nrow){
        result <- paste0(result, mat[i, j], if (j == nrow) " \\\\ \n" else " & ")
      }
    }
  } else if (is.character(nrow)){
    vdots <- paste0("\\vdots",
                    paste(rep(" ", nchar(symbol) - 1), collapse = ""))
    row.subscripts <- c("1", "2", "", nrow)
    if (is.character(ncol)){
      vdots <- paste0(vdots, " & ", vdots, " & ",
                      if (nrow != ncol) "       & " else "\\ddots & ",
                      vdots, " \\\\ \n")
      for (i in 1:4){
        result <- paste0(result, "  ")
        if (i == 3){
          result <- paste0(result, vdots)
          next
        }
        for (j in 1:4){
          result <- paste0(result, row.elements[j], left.sub[j],
                           if (j !=3) paste0(row.subscripts[i], comma),
                           col.subscripts[j],
                           right.sub[j], post.element[j])
        }
      }
    } else {
      vdots <- paste0(paste(rep(vdots, ncol), collapse = " & "), " \\\\ \n")
      for (i in 1:4){
        result <- paste0(result, "  ")
        if (i == 3){
          result <- paste0(result, vdots)
          next
        }
        for (j in 1:ncol){
          result <- paste0(result,
                           paste0(symbol, "_{", row.subscripts[i], if (ncol > 1) paste0(comma, j), "}",
                                  if (j == ncol) " \\\\ \n" else " & ")
          )
        }
      }
    }
  } else if (is.character(ncol)){
      for (i in 1:nrow){
        result <- paste0(result, "  ")
        for (j in 1:4){
          result <- paste0(result, row.elements[j], left.sub[j],
                           if (j !=3 && nrow > 1) paste0(i, comma),
                           col.subscripts[j],
                           right.sub[j], post.element[j])
        }
      }
    } else {
      for (i in 1:nrow){
        result <- paste0(result, "  ")
        for (j in 1:ncol){
          result <- paste0(result, symbol, "_{", if (nrow > 1) i,
                           if (nrow > 1 && ncol > 1) comma,
                           if (ncol > 1) j, "}",
                           if (j == ncol) " \\\\ \n" else " & ")
        }
      }
    }
  result <- paste0(result, "\\end{", matrix, "}",
                   if (!missing(exponent)) paste0("^{", exponent, "}"),
                   if (!isFALSE(transpose)) paste0("^", transpose),
                   "\n")
  if (print) cat(result)
  invisible(result)
}
