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
#' The function prints the resulting code to the console, or, using \code{\link[clipr]{write_clip}}, can be copied to the console,
#' or can be used used in a markdown chunk in a \code{Rmd} or \code{qmd} document, e.g,
#' \preformatted{
#' ```{r results = "asis"}
#' symbolicMatrix("\\lambda", nrow=3, ncol=3,
#'                diag=TRUE,
#'                lhs = "\\boldsymbol{\\Lambda}")
#' ```
#' }
#'
#' @details
#' This implementation assumes that the LaTeX \code{amsmath} package will be available because it uses the shorthands
#' \code{\\begin{pmatrix}}, ... rather than \code{\\left( \\begin{array} ... \\end{array} \\right)}.
#'
#' This function is experimental. Other features may be added.  E.g., it would be nice to
#'
#' * Specify exponents for the matrix elements, e.g, a diagonal matrix of square roots of eigenvalues, \code{\\lambda_i^{1/2}}
#' * Specify "accents" for the symbols, e.g., when you want the elements to be \code{\\widehat{\\beta}_{ij}}.
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
#' @param diag   logical; if \code{TRUE}, off-diagonal elements are all 0 (and \code{nrow} must == \code{ncol})
#' @param comma  logical; if \code{TRUE}, commas are inserted between row and column subscripts
#' @param exponent if specified, e.g., "-1", or "1/2",  the exponent is applied to the matrix
#' @param transpose if TRUE, the transpose symbol "\\top" is appended to the matrix; may
#'               also be a character string, e.g., \code{"T"}, \code{"\\prime"}, \code{"\textsf{T}"}
#' @param lhs    optional LaTeX expression, e.g, "\\boldsymbol{\\Lamda}", for left-hand side of an equation
#'               with the matrix on the right-hand side.
#' @param print  logical; print the LaTeX code for the matrix on the console?; default: \code{TRUE}
#'
#' @returns Returns invisibly the LaTeX representation of the matrix as a character sting.
#'        Use \code{cat()} to display it at the console, or \code{clipr::write_clip()} to copy it to the clipboard
#'
#' @author John Fox
#' @export
#' @examples
#' symbolicMatrix()
#'
#' # return value
#' mat <- symbolicMatrix(print = FALSE)
#' str(mat)
#' cat(mat)
#' # copy to clipboard
#' clipr::write_clip(mat)
#'
#' # numeric rows/cols
#' symbolicMatrix(ncol=3)
#' symbolicMatrix(nrow=4)
#' symbolicMatrix(nrow=4, ncol=4)
#'
#' # diagonal matrices
#' symbolicMatrix(nrow=3, ncol=3, diag=TRUE)
#' symbolicMatrix(nrow="n", ncol="n", diag=TRUE)
#'
#' # commas, exponents, transpose
#' symbolicMatrix("\\beta", comma=TRUE, exponent="-1")
#' symbolicMatrix("\\beta", comma=TRUE, transpose=TRUE)
#' symbolicMatrix("\\beta", comma=TRUE, exponent="-1", transpose=TRUE)

symbolicMatrix <- function(
      symbol="x",
      nrow="n",
      ncol="m",
      matrix="pmatrix",
      diag=FALSE,
      comma=FALSE,
      exponent,
      transpose=FALSE,
      lhs,
      print=TRUE){

  # Args:
  #   symbol: for matrix elements, character string
  #   nrow: number of rows, can be a character
  #   ncol: number of columns, can be a character
  #   matrix: LaTeX matix environment
  #   diag: if TRUE, off-diagonal elements are all 0 (and nrow must == ncol)
  #   comma: if TRUE, commas are inserted between row and column subscripts
  #   exponent: if specified, e.g., "-1", the exponent is applied to the matrix
  #   transpose: if TRUE, the transpose symbol "\\top" is appended to the
  #              matrix; may also be a character, e.g., "T".
  #   lhs: optional LaTeX expression, e.g, "\\boldsymbol{\\Lamda}", for
  #        left-hand side of an equation with the matrix on the right-hand side.
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

  result <- paste0(if (!missing(lhs)) paste0(lhs, " = "),
    "\\begin{", matrix, "} \n")

  if (diag){
    zero <- paste0("0", paste(rep(" ", nchar(symbol) + 3), collapse=""))
    if (nrow != ncol) stop("nrow and ncol must be the same if diag = TRUE")
    if (is.numeric(nrow)){
      mat <- matrix(zero, nrow, nrow)
      diag(mat) <- paste0(symbol, "_{", 1:nrow, "}")
    } else {
      mat <- matrix(zero, 4, 4)
      mat[3, ] <- paste0("\\vdots",
                         paste0(paste(rep(" ", max(0, nchar(symbol) - 2)),
                                      collapse = "")))
      mat[, 3] <- paste0("\\cdots",
                         paste0(paste(rep(" ", max(0, nchar(symbol) - 2)),
                                      collapse = "")))
      mat[3, 3] <- paste0("\\ddots",
                          paste0(paste(rep(" ", max(0, nchar(symbol) - 2)),
                                       collapse = "")))
      mat[cbind(c(1, 2, 4), c(1, 2, 4))] <-
        paste0(symbol, c("_{1}", "_{2}", paste0("_{", nrow, "}")))
    }
    if (is.character(nrow)) nrow <- 4
    for (i in 1:nrow){
      result <- paste0(result, "  ")
      for (j in 1:nrow){
        result <- paste0(result, mat[i, j],
                         if (j == nrow) " \\\\ \n" else " & ")
      }
    }
  } else if (is.character(nrow)){
    vdots <- paste0("\\vdots",
                    paste0(paste(rep(" ", nchar(symbol) - 1), collapse = ""),
                           if (comma == ",") " "))
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
                           paste0(symbol, "_{", row.subscripts[i],
                                  if (ncol > 1) paste0(comma, j), "}",
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
