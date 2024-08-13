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
#' You can print the resulting LaTeX code to the console. When the result is assigned to a variable,
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
#' You may need to use \code{extra_dependencies: ["amsmath"]} in your YAML header of a \code{Rmd} or \code{qmd} file.
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
#' The accessor functions \code{getLatex()}, \code{getBody()}, \code{getWrapper()},
#' \code{getDim()}, \code{getNrow()}, and \code{getNcol()} may be used to retrieve
#' components of the resturned object.
#'
#' @param symbol name for matrix elements, character string. For LaTeX symbols,
#'        the backslash must be doubled because it is an escape character in R.
#'        That is, you must use  \code{symbol = "\\\\beta"} to get \eqn{\beta}. Alternatively, this can be an
#'        R matrix object, containing LaTeX code for the elements. For a row or column vector, use
#'        \code{matrix(..., nrow=1)} or \code{matrix(..., ncol=1)}
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
#' @param sparse logical; if \code{TRUE} replace 0's with empty characters to print a sparse matrix
#' @param zero.based logical 2-vector; start the row and/or column indices at 0 rather than 1;
#'   the default is \code{c(FALSE, FALSE)}; applies only if \code{nrow} is character-valued
#' @param end.at if row or column indices start at 0, should they end at \code{n - 1} and
#'   \code{m - 1} or at \code{n} and \code{m}? (where \code{n} and \code{m} represent the
#'   characters used to denote the number of rows and columns, respectively);
#'   the default is \code{c("n - 1", "m - 1")}
#' @param comma  logical; if \code{TRUE}, commas are inserted between row and column subscripts, as in
#'               \code{x_{1,1}}; the default is \code{FALSE} except for zero-based indices.
#' @param exponent if specified, e.g., \code{"-1"}, or \code{"1/2"},  the exponent is applied to the matrix
#' @param transpose if \code{TRUE}, the transpose symbol \code{"\\top"} is appended to the matrix; this may
#'               also be a character string, e.g., \code{"T"}, \code{"\\prime"}, \code{"\textsf{T}"} are
#'               commonly used.
#' @param show.size logical; if \code{TRUE} shows the order of the matrix as an appended subscript.
#' @param fractions logical; if \code{TRUE}, try to express non-integers as rational numbers, using the \code{\link[MASS]{fractions}}
#'                  function.
#' @param digits for a numeric matrix, number of digits to display;
#                the default is taken from \code{getOption("digits") - 2};
#                the function sets \code{digits = 0} if the elements of
#                \code{symbol} are all integers.
#' @param prefix optional character string to be pre-pended to each matrix element, e.g, to wrap each
#'               element in a function like \code{"\\sqrt"} (but add braces)
#' @param suffix optional character string to be appended to each matrix element, e.g., for exponents
#'               on each element
#' @param lhs    character; an optional LaTeX expression, e.g, "\code{\\boldsymbol{\\Lamda}}", for left-hand
#'               side of an equation
#'               with the generated matrix on the right-hand side.
#' @param onConsole if \code{TRUE}, the default, print the LaTeX code for
#'                  the matrix on the R console.
#'
#' @returns \code{symbolicMatrix()} returns an object of class \code{"symbolicMatrix"}
#'          which contains the LaTeX representation of the matrix as a character string,
#'          along with some other information. The slots
#'          in the returned object are named \code{"matrix"} (the LaTeX representation of the
#'          matrix); \code{"dim"} (\code{nrow} and \code{ncol}); \code{"body"} (a character
#'          matrix of LaTeX expressions for the cells of the matrix);
#'          \code{"wrapper"}(the beginning and ending lines for the
#'          LaTeX matrix environment).
#'
#' @author John Fox
#' @seealso \code{\link{matrix2latex}}, \code{\link[clipr]{write_clip}}
#' @export
#' @examples
#' symbolicMatrix()
#'
#' # return value
#' mat <- symbolicMatrix()
#' str(mat)
#' cat(getLatex(mat))
#' # copy to clipboard
#' #clipr::write_clip(mat)  # this can't be done in non-interactive mode
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
#' symbolicMatrix(nrow="n", ncol="n", diag=TRUE, sparse=TRUE)
#'
#' # commas, exponents, transpose
#' symbolicMatrix("\\beta", comma=TRUE, exponent="-1")
#' symbolicMatrix("\\beta", comma=TRUE, transpose=TRUE)
#' symbolicMatrix("\\beta", comma=TRUE, exponent="-1", transpose=TRUE)
#'
#' # for a row/column vector, wrap in matrix()
#' symbolicMatrix(matrix(LETTERS[1:4], nrow=1))
#' symbolicMatrix(matrix(LETTERS[1:4], ncol=1))
#'
#' # represent the SVD, X = U D V'  symbolically
#' X <- symbolicMatrix("x", "n", "p")
#' U <- symbolicMatrix("u", "n", "k")
#' D <- symbolicMatrix("\\lambda", "k", "k", diag=TRUE)
#' V <- symbolicMatrix("v", "k", "p", transpose = TRUE)
#' cat("\\mathrm{SVD:}\n", getLatex(X), "=\n", getLatex(U),
#'     getLatex(D), getLatex(V))
#'
#' # specify left hand side
#' symbolicMatrix("\\lambda", 3, 3, diag=TRUE, lhs = "\\boldsymbol{\\Lambda}")
#' symbolicMatrix("\\lambda", 3, 3, diag=TRUE, sparse=TRUE,
#'   lhs = "\\boldsymbol{\\Lambda}")
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
#' symbolicMatrix(diag(3), lhs = "\\mathbf{I}_3", sparse=TRUE)
#'
#' # prefix / suffix
#' symbolicMatrix(prefix="\\sqrt{", suffix="}")
#' symbolicMatrix(suffix="^{1/2}")
#'
#' # show size (order) of a matrix
#' symbolicMatrix(show.size=TRUE)
#' symbolicMatrix(nrow=3, ncol=4, show.size=TRUE)
#'
#' # handling fractions
#' m <- matrix(3/(1:9), 3, 3)
#' symbolicMatrix(m)
#' symbolicMatrix(m, digits=2)
#' symbolicMatrix(m, fractions=TRUE)
#'
#' # zero-based indexing
#' symbolicMatrix(zero.based=c(TRUE, TRUE))


symbolicMatrix <- function(
    symbol="x",
    nrow="n",
    ncol="m",
    matrix="pmatrix",
    diag=FALSE,
    sparse=FALSE,
    zero.based=c(FALSE, FALSE),
    end.at=c("n - 1", "m - 1"),
    comma=any(zero.based),
    exponent,
    transpose=FALSE,
    show.size=FALSE,
    digits=getOption("digits") - 2,
    fractions=FALSE,
    prefix="",
    suffix="",
    lhs
){

  latexFraction <- function(x){
    negative <- grepl("-", x)
    if (negative) x <- sub("-", "", x)
    if (grepl("/", x)){
      x <- sub("/", "}{", x)
      x <- paste0("\\frac{", x, "}")
    }
    x <- if (negative) paste0("-", x) else paste0(negatives, x)
    x
  }

  end.at.n.minus.1 <- gsub(" ", "", end.at) == c("n-1", "m-1")

  if (is.numeric(nrow) && zero.based[1]){
    stop("zero-based indexing not available for numeric 'nrow'")
  }
  if (is.numeric(ncol) && zero.based[2]){
    stop("zero-based indexing not available for numeric 'ncol'")
  }

  if (isTRUE(transpose)) transpose <- "\\top"
  if (!missing(exponent) && !isFALSE(transpose)){
    exponent <- paste0("{", exponent, "^", transpose, "}")
    transpose <- FALSE
  }

  # start composing output string:

  result <- paste0(if (fractions) "\\renewcommand*{\\arraystretch}{1.5} \n",
                   if (!missing(lhs)) paste0(lhs, " = \n"),
                   "\\begin{", matrix, "} \n"
  )

  # matrix input:

  if (is.matrix(symbol)){
    if (is.numeric(symbol)){
      if (is.null(digits) && all(trunc(symbol) == symbol) ) digits <- 0
      if (fractions) {
        symbol <- as.character(Fractions(symbol))
        negatives <- if (any(grepl("-", symbol))) "\\phantom{-}" else ""
        for (i in 1:nrow(symbol)){
          for (j in 1:ncol(symbol)){
            symbol[i, j] <- latexFraction(symbol[i, j])
          }
        }
      } else {
        symbol <- format(symbol, digits=digits)
      }
    }
    mat <- matrix(as.character(symbol), nrow(symbol), ncol(symbol))
    width <- apply(nchar <- nchar(mat), 2, max)
    nrow <- nr <- nrow(mat)
    ncol <- nc <- ncol(mat)
    for (i in 1:nr){
      for (j in 1:nc){
        mat[i, j] <- paste0(prefix, mat[i, j], suffix,
                            paste(rep(" ", width[j] - nchar[i, j]),
                                  collapse=""))
        result <- paste0(result, mat[i, j], if (j == nc) " \\\\ \n" else " & " )
      }
    }

  } else {

    # character symbol supplied to construct matrix elements:

    if (!(is.character(symbol) && is.vector(symbol)
          && length(symbol) == 1))
      stop("'symbol' must be a single character string or a matrix. Hint: wrap a vector in matrix(), with nrow=1 or ncol=1.")

    if (is.numeric(nrow)){
      if (round(nrow) != nrow || nrow <= 0)
        stop("nrow is not a positive whole number")
    }

    if (is.numeric(ncol)){
      if (round(ncol) != ncol || ncol <= 0)
        stop("ncol is not a positive whole number")
    }

    comma <- if (comma) "," else ""

    row.elements <- c(symbol, symbol, "\\cdots", symbol) # row without subscripts

    col.subscripts <- if (!zero.based[2]) { # subscripts for a column
      c("1", "2", "", ncol)
    } else {
      c("0", "1", "", if (is.numeric(ncol)) ncol - 1
        else paste0(ncol, if (end.at.n.minus.1[2]) " - 1" else ""))
    }

    left.sub <- c("_{", "_{", "", "_{") # start of subscript
    right.sub <- c("}", "}", "", "}") # end of subscript
    post.element <- c(" & ", " & ", " & ", " \\\\ \n") # cell separator, end of row

    if (diag){

      # diagonal matrix:

      zero <- paste0("0", paste(rep(" ",
                                    nchar(symbol) + 3 + nchar(prefix) + nchar(suffix)),
                                collapse=""))
      if (nrow != ncol) stop("nrow and ncol must be the same if diag = TRUE")
      if (is.numeric(nrow)){
        mat <- matrix(zero, nrow, nrow)
        diag(mat) <- paste0(prefix, symbol, "_{",
                            (!zero.based[1]):(nrow - zero.based[1]),
                            "}", suffix)
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
                 symbol,
                 if (!zero.based[1]){
                   c("_{1}", "_{2}", paste0("_{", nrow, "}"))
                 } else {
                   c("_{0}", "_{1}", paste0("_{", nrow,
                                            if (end.at.n.minus.1[1]) " - 1" else "",
                                            "}"))
                 },
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

      # non-numeric number of rows:

      vdots <- paste0("\\vdots",
                      paste0(paste(rep(" ",
                                       nchar(symbol) + nchar(prefix) + nchar(suffix) - 1),
                                   collapse = ""),
                             if (comma == ",") " "))
      row.subscripts <- if (!zero.based[1]){
        c("1", "2", "", nrow)
      } else {
        c("0", "1", "", paste0(nrow, if (end.at.n.minus.1[1]) " - 1" else ""))
      }

      if (is.character(ncol)){

        # non-numeric number of rows, non-numeric number of columns:

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

        # non-numeric number of rows, numeric number of columns:

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
                                    if (ncol > 1) paste0(comma,
                                                         j - zero.based[2]), "}",
                                    suffix,
                                    if (j == ncol) " \\\\ \n" else " & ")
            )
          }
        }
      }

    } else if (is.character(ncol)){

      # numeric number of rows, non-numeric number of columns:

      for (i in 1:nrow){
        result <- paste0(result, "  ")
        for (j in 1:4){
          result <- paste0(result,if (j != 3) prefix,
                           row.elements[j], left.sub[j],
                           if (j != 3) paste0(i - zero.based[1], comma),
                           col.subscripts[j],
                           right.sub[j],  if (j != 3) suffix,
                           post.element[j])
        }
      }

    } else {

      # numeric number of rows, numeric number of columns:

      is_scalar <- nrow == 1 && ncol == 1

      for (i in 1:nrow){
        result <- paste0(result, "  ")
        for (j in 1:ncol){
          result <- paste0(result, prefix, symbol,
                           if(!is_scalar) "_{",
                           if (nrow > 1) i - zero.based[1],
                           if (nrow > 1 && ncol > 1) comma,
                           if (ncol > 1) j - zero.based[2],
                           if(!is_scalar) "}", suffix,
                           if (j == ncol) " \\\\ \n" else " & ")
        }
      }
    }
  }

  # complete output string, adding optional decorations:

  mat.result <- paste0(result, "\\end{", matrix, "}",
                       if (show.size) paste0("_{(",
                                             nrow,
                                             if (zero.based[1] && !end.at.n.minus.1[1]) " + 1",
                                             " \\times ",
                                             ncol,
                                             if (zero.based[2] && !end.at.n.minus.1[2]) " + 1",
                                             ")}" ),
                       if (!missing(exponent)) paste0("^{", exponent, "}"),
                       if (!isFALSE(transpose)) paste0("^", transpose))

  x.mat <- strsplit(mat.result, "\\n")[[1]]
  pick <- c(1, length(x.mat))
  wrapper <- x.mat[pick] # LaTeX matrix environment
  body <- x.mat[-pick]
  body <- gsub('\\\\\\\\', '', body)
  # body <- gsub(' ', '', body)
  splt <- sapply(body, function(x.mat) strsplit(x.mat, '&'))
  nrow.x <- length(splt)
  body <- unname(do.call(rbind, splt)) # matrix of LaTeX cells
  body <- sub(" *$", "", sub("^ *", "", body))
  if(sparse)
      mat.result <- gsub('[[:space:]]+0[[:space:]]+', ' ', mat.result)

  # "symbolicMatrix" object:

  result <- list(matrix = mat.result,
                 dim = c(nrow, ncol),
                 body = body,
                 wrapper = wrapper)
  class(result) <- 'symbolicMatrix'
  result
}

# accessor functions:

#' @rdname symbolicMatrix
#' @export
getLatex <- function(x, ...){
  UseMethod("getLatex")
}

#' @rdname symbolicMatrix
#' @export
getLatex.symbolicMatrix <- function(x, ...){
  x$matrix
}

#' @rdname symbolicMatrix
#' @export
getBody <- function(x, ...){
  UseMethod("getBody")
}

#' @rdname symbolicMatrix
#' @export
getBody.symbolicMatrix <- function(x, ...){
  x$body
}

#' @rdname symbolicMatrix
#' @export
getWrapper <- function(x, ...){
  UseMethod("getWrapper")
}

#' @rdname symbolicMatrix
#' @export
getWrapper.symbolicMatrix <- function(x, ...){
  x$wrapper
}

#' @rdname symbolicMatrix
#' @export
Dim <- function(x, ...){
  UseMethod("Dim")
}

#' @rdname symbolicMatrix
#' @export
Dim.symbolicMatrix <- function(x, ...){
  x$dim
}

#' @rdname symbolicMatrix
#' @export
Nrow <- function(x, ...){
  UseMethod("Nrow")
}

#' @rdname symbolicMatrix
#' @export
Nrow.symbolicMatrix <- function(x, ...){
  (x$dim)[1L]
}

#' @rdname symbolicMatrix
#' @export
Ncol <- function(x, ...){
  UseMethod("Ncol")
}

#' @rdname symbolicMatrix
#' @export
Ncol.symbolicMatrix <- function(x, ...){
  (x$dim)[2L]
}

#' @param x a \code{"symbolicMatrix"} object
#' @param ...  for compatibility with the \code{print()} generic function, ignored

# print() method:
#' @rdname symbolicMatrix
#' @export
print.symbolicMatrix <- function(x, onConsole=TRUE,  ...){
  if (onConsole) cat(getLatex(x))
  invisible(x)
}
