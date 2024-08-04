# JF version of 8 Aug 24, w/ prefix, suffix
# TODO: Trap the case of a vector symbol, e.g, symbolicMatrix(1:4)

#' Create a Symbolic Matrix for LaTeX
#'
#' @description
#' Constructs the LaTeX code for a symbolic matrix, whose elements are a \code{symbol}, with row and column subscripts.
#' For example:
#' \preformatted{
#'  \\begin{pmatrix}
#'    x_{11}  & x_{12}  & \\dots  & x_{1m}  \\
#'    x_{21}  & x_{22}  & \\dots  & x_{2m}  \\
#'    \\vdots & \\vdots & \\ddots & \\vdots \\
#'    x_{n1}  & x_{n2}  & \\dots  & x_{nm}
#'  \\end{pmatrix}
#'  }
#'
#'  When rendered in LaTeX, this produces:
#'  \deqn{
#'  \begin{pmatrix}
#'    x_{11} & x_{12} & \cdots & x_{1m} \\
#'    x_{21} & x_{22} & \cdots & x_{2m} \\
#'    \vdots & \vdots &        & \vdots \\
#'    x_{n1} & x_{n2} & \cdots & x_{nm} \\
#'  \end{pmatrix}
#'  }
#'
#' % \figure{man/figures/symbMat-x.png}{options: width=150 alt="LaTeX result for the symbolic n x m matrix"}.
#'
#' Alternatively, instead of characters,
#' the number of rows and/or columns can be \bold{integers}, generating a matrix of given size.
#'
#' As well, instead of a character for the matrix \code{symbol}, you can supply a \bold{matrix} of arbitrary character
#' strings (in LaTeX notation), and these will be used as the elements of the matrix.
#'
#' The function prints the resulting code to the console (by default). When the result is assigned to variable,
#' you can send it to the clipboard using \code{\link[clipr]{write_clip}}. Perhaps most convenient of all,
#' the function can be used used in a markdown chunk in a \code{Rmd} or \code{qmd} document, e.g,
#'
#' \preformatted{
#' ```{r results = "asis"}
#' symbolicMatrix("\\lambda", nrow=2, ncol=2,
#'                diag=TRUE,
#'                lhs = "\\boldsymbol{\\Lambda}")
#' ```
#' }
#'
#' This generates
#' \deqn{
#'  \boldsymbol{\Lambda} = \begin{pmatrix}
#'  \lambda_{1} & 0           \\
#'  0           & \lambda_{2} \\
#'  \end{pmatrix}
#'  }
#'
#' @details
#' This implementation assumes that the LaTeX \code{amsmath} package will be available because it uses the shorthands
#' \code{\\begin{pmatrix}}, ... rather than
#' \preformatted{
#' \\left(
#'   \\begin{array}(ccc)
#'   ...
#'   \\end{array}
#' \\right)
#' }.
#'
#' You can actually supply a numeric matrix as the \code{symbol}, but the result will not be pretty
#' unless the elements are integers or are rounded. For a LaTeX representation of general numeric matrices, use
#' \code{\link{matrix2latex}}.
#'
#' % This function is \bold{experimental}. Other features may be added.  E.g., it would be nice to:
#'
#' %\itemize{
#' % \item Specify exponents for the \bold{matrix elements}, e.g, a diagonal matrix of square roots of eigenvalues,
#' % \code{\\lambda_i^{1/2}} giving \eqn{\lambda_i^{1/2}}
#' %}
#'
#' @param symbol name for matrix elements, character string. For LaTeX symbols,
#'        the backslash must be doubled because it is an escape character in R.
#'        That is, you must use  \code{symbol = "\\\\beta"} to get \eqn{\beta}. Alternatively, this can be an
#'        R matrix object, containing LaTeX code for the elements.
#' @param nrow   Number of rows, a single character representing rows symbolically, or an integer, generating
#'               that many rows.
#' @param ncol   Number of columns, a single character representing columns symbolically, or an integer, generating
#'               that many columns.
#' @param matrix Character string giving the LaTeX matrix environment used in \code{\\begin{}}, \code{\\end{}}. Typically one of:
#' \describe{
#'    \item{\code{"pmatrix"}}{uses parentheses: \code{"(", ")"}}
#'    \item{\code{"bmatrix"}}{uses square brackets: \code{"[", "]"}}
#'    \item{\code{"Bmatrix"}}{uses braces: \code{"{", "}"}}
#'    \item{\code{"vmatrix"}}{uses vertical bars: \code{"|", "|"}}
#'    \item{\code{"Vmatrix"}}{uses double vertical bars: \code{"||", "||"}}
#'    \item{\code{"matrix"}}{generates a plain matrix without delimeters}
#' }
#'
#' @param diag   logical; if \code{TRUE}, off-diagonal elements are all 0 (and \code{nrow} must == \code{ncol})
#' @param comma  logical; if \code{TRUE}, commas are inserted between row and column subscripts, as in
#'               \code{x_{1,1}}.
#' @param exponent if specified, e.g., "-1", or "1/2",  the exponent is applied to the matrix
#' @param transpose if TRUE, the transpose symbol "\\top" is appended to the matrix; this may
#'               also be a character string, e.g., \code{"T"}, \code{"\\prime"}, \code{"\textsf{T}"} are
#'               commonly used.
#' @param prefix optional character string to be pre-pended to each matrix element, e.g, to wrap each
#'               element in a function like \code{"\\sqrt"} (but add braces)
#' @param suffix optional character string to be appended to each matrix element, e.g., for exponents
#'               on each element
#' @param lhs    character; an optional LaTeX expression, e.g, "\code{\\boldsymbol{\\Lamda}}", for left-hand
#'               side of an equation
#'               with the generated matrix on the right-hand side.
#' @param print  logical; print the LaTeX code for the matrix on the console?; default: \code{TRUE}
#'
#' @returns Returns invisibly the LaTeX representation of the matrix as a character string.
#'        If you assign to a variable, you can use \code{\link[clipr]{write_clip}} to copy it to the clipboard.
#'
#'        As a side-effect, by default (unless \code{print = FALSE}) the function uses
#'        \code{cat()} to display the result at the console.
#'
#' @author John Fox
#' @seealso \code{\link{matrix2latex}}, \code{\link[clipr]{write_clip}}
#' @export
#' @examples
#' symbolicMatrix()
#'
#' # return value
#' mat <- symbolicMatrix(print = FALSE)
#' str(mat)
#' cat(mat)
#' # copy to clipboard
#' #clipr::write_clip(mat)    # this can't be done in non-interactive mode
#'
#' # can use a complex symbol
#' symbolicMatrix("\\widehat{\\beta}", 2, 4)
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
#'
#' # represent the SVD, X = U D V'  symbolically
#' X <- symbolicMatrix("x", "n", "p")
#' U <- symbolicMatrix("u", "n", "k")
#' D <- symbolicMatrix("\\lambda", "k", "k", diag=TRUE)
#' V <- symbolicMatrix("v", "k", "p", transpose = TRUE)
#' cat("\\mathrm{SVD:}\n", X, "=\n", U, D, V)
#'
#' # specify left hand side
#' symbolicMatrix("\\lambda", 3, 3, diag=TRUE, lhs = "\\boldsymbol{\\Lambda}")
#'
#' # supply a matrix for 'symbol'
#' m <- matrix(c(
#'   "\\alpha", "\\beta",
#'   "\\gamma", "\\delta",
#'   "\\epsilon", "\\pi",
#'   0 , 0), 4, 2, byrow=TRUE)
#' symbolicMatrix(m)
#'
#' # Identity matrix
#' symbolicMatrix(diag(3), lhs = "\\mathbf{I}_3")
#'
#' # prefix / suffix
#' symbolicMatrix(prefix="\\sqrt{", suffix="}")
#' symbolicMatrix(suffix="^{1/2}")
#'


symbolicMatrix <- function(
    symbol="x",
    nrow="n",
    ncol="m",
    matrix="pmatrix",
    diag=FALSE,
    comma=FALSE,
    exponent,
    transpose=FALSE,
    prefix="",
    suffix="",
    lhs,
    print=TRUE){
  
  # Args:
  #   symbol: for matrix elements, character string; alternative a matrix
  #   nrow: number of rows, can be a character
  #   ncol: number of columns, can be a character
  #   matrix: LaTeX matix environment
  #   diag: if TRUE, off-diagonal elements are all 0 (and nrow must == ncol)
  #   comma: if TRUE, commas are inserted between row and column subscripts
  #   exponent: if specified, e.g., "-1", the exponent is applied to the matrix
  #   transpose: if TRUE, the transpose symbol "\\top" is appended to the
  #              matrix; may also be a character, e.g., "T".
  #   prefix: optional character string to be pre-pended to each matrix element
  #   suffix: optional character string to be appended to each matrix element
  #   lhs: optional LaTeX expression, e.g, "\\boldsymbol{\\Lamda}", for
  #        left-hand side of an equation with the matrix on the right-hand side.
  #   print: print the LaTeX code for the matrix on the console
  
  if (isTRUE(transpose)) transpose <- "\\top"
  if (!missing(exponent) && !isFALSE(transpose)){
    exponent <- paste0("{", exponent, "^", transpose, "}")
    transpose <- FALSE
  }
  
  result <- paste0(if (!missing(lhs)) paste0(lhs, " = \n"),
                   "\\begin{", matrix, "} \n")
  
  if (is.matrix(symbol)){
    mat <- matrix(as.character(symbol), nrow(symbol), ncol(symbol))
    width <- apply(nchar <- nchar(mat), 2, max)
    nr <- nrow(mat)
    nc <- ncol(mat)
    for (i in 1:nr){
      for (j in 1:nc){
        mat[i, j] <- paste0(prefix, mat[i, j], suffix,
                            paste(rep(" ", width[j] - nchar[i, j]),
                                  collapse=""))
        result <- paste0(result, mat[i, j], if (j == nc) " \\\\ \n" else " & " )
      }
    }
    
  } else {
    
    if (!(is.character(symbol) && is.vector(symbol) 
          && length(symbol) == 1))
      stop("symbol must be a single character string or a matrix")
    
    if (is.numeric(nrow)){
      if (round(nrow) != nrow || nrow <= 0)
        stop("nrow is not a positive whole number")
    }
    
    if (is.numeric(ncol)){
      if (round(ncol) != ncol || ncol <= 0)
        stop("ncol is not a positive whole number")
    }
    
    comma <- if (comma) "," else ""
    
    row.elements <- c(symbol, symbol, "\\cdots", symbol)
    col.subscripts <- c("1", "2", "", ncol)
    left.sub <- c("_{", "_{", "", "_{")
    right.sub <- c("}", "}", "", "}")
    post.element <- c(" & ", " & ", " & ", " \\\\ \n")
    
    if (diag){
      zero <- paste0("0", paste(rep(" ",
                                    nchar(symbol) + 3 + nchar(prefix) + nchar(suffix)),
                                collapse=""))
      if (nrow != ncol) stop("nrow and ncol must be the same if diag = TRUE")
      if (is.numeric(nrow)){
        mat <- matrix(zero, nrow, nrow)
        diag(mat) <- paste0(prefix, symbol, "_{", 1:nrow, "}", suffix)
      } else {
        mat <- matrix(zero, 4, 4)
        mat[3, ] <- paste0("\\vdots",
                           paste0(paste(rep(" ",
                                            max(0, nchar(symbol) - 2 + nchar(prefix)
                                                + nchar(suffix))),
                                        collapse = "")))
        mat[, 3] <- paste0("\\cdots",
                           paste0(paste(rep(" ", max(0, nchar(symbol) - 2)),
                                        collapse = "")))
        mat[3, 3] <- paste0("\\ddots",
                            paste0(paste(rep(" ", max(0, nchar(symbol) - 2)),
                                         collapse = "")))
        mat[cbind(c(1, 2, 4), c(1, 2, 4))] <-
          paste0(prefix,
                 symbol, c("_{1}", "_{2}", paste0("_{", nrow, "}")),
                 suffix)
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
                      paste0(paste(rep(" ",
                                       nchar(symbol) + nchar(prefix) + nchar(suffix) - 1),
                                   collapse = ""),
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
            result <- paste0(result, if (j != 3) prefix, row.elements[j], left.sub[j],
                             if (j !=3) paste0(row.subscripts[i], comma),
                             col.subscripts[j],
                             right.sub[j], if (j != 3) suffix, post.element[j])
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
                             paste0(prefix,
                                    symbol, "_{", row.subscripts[i],
                                    if (ncol > 1) paste0(comma, j), "}",
                                    suffix,
                                    if (j == ncol) " \\\\ \n" else " & ")
            )
          }
        }
      }
      
    } else if (is.character(ncol)){
      for (i in 1:nrow){
        result <- paste0(result, "  ")
        for (j in 1:4){
          result <- paste0(result,if (j != 3) prefix,
                           row.elements[j], left.sub[j],
                           if (j != 3) paste0(i, comma),
                           col.subscripts[j],
                           right.sub[j],  if (j != 3) suffix,
                           post.element[j])
        }
      }
      
    } else {
      for (i in 1:nrow){
        result <- paste0(result, "  ")
        for (j in 1:ncol){
          result <- paste0(result, prefix, symbol, "_{", if (nrow > 1) i,
                           if (nrow > 1 && ncol > 1) comma,
                           if (ncol > 1) j, "}", suffix,
                           if (j == ncol) " \\\\ \n" else " & ")
        }
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

