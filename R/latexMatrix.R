#' Create and Manipulate LaTeX Representations of Matrices
#'
#' @description
#' The purpose of the \code{latexMatrix()} function is to facilitate the preparation
#' of LaTeX and Markdown documents that include matrices. The function generates the
#' the LaTeX code for matrices of various types programmatically. The objects produced
#' by the function can also be manipulated, e.g., with standard arithmetic functions and operators:
#' See \code{\link{latexMatrixOperations}}.
#' 
#' The \code{latexMatrix()} function can construct the LaTeX code for a symbolic matrix, whose elements are a \code{symbol}, with row and column subscripts.
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
#' strings (in LaTeX notation) or numbers, and these will be used as the elements of the matrix.
#'
#' You can print the resulting LaTeX code to the console. When the result is assigned to a variable,
#' you can send it to the clipboard using \code{\link[clipr]{write_clip}()}. Perhaps most convenient of all,
#' the function can be used used in a markdown chunk in a \code{Rmd} or \code{qmd} document, e.g,
#'
#' \preformatted{
#' ```{r results = "asis"}
#' latexMatrix("\\lambda", nrow=2, ncol=2,
#'                diag=TRUE)
#' ```
#' }
#'
#' This generates
#' \deqn{
#'  \begin{pmatrix}
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
#' }
#'
#' You may need to use \code{extra_dependencies: ["amsmath"]} in your YAML header of a \code{Rmd} or \code{qmd} file.
#'
#' You can supply a numeric matrix as the \code{symbol}, but the result will not be pretty
#' unless the elements are integers or are rounded. For a LaTeX representation of general numeric matrices, use
#' \code{\link{matrix2latex}}.
#' 
#' The \code{partition()} function modifies (only) the printed LaTeX representation of a \code{"latexMatrix"}
#' object to include partition lines by rows and/or columns.
#'
#' The accessor functions \code{getLatex()}, \code{getBody()}, \code{getWrapper()},
#' \code{getDim()}, \code{getNrow()}, and \code{getNcol()} may be used to retrieve
#' components of the returned object.
#' 
#' Various functions and operators for \code{"latexMatrix"} objects are
#' documented separately; see, \code{\link{latexMatrixOperations}}.
#'
#' @param symbol name for matrix elements, character string. For LaTeX symbols,
#'        the backslash must be doubled because it is an escape character in R.
#'        That is, you must use  \code{symbol = "\\\\beta"} to get \eqn{\beta}. Alternatively, this can be an
#'        R matrix object, containing numbers or LaTeX code for the elements. For a row or column vector, use
#'        \code{matrix(..., nrow=1)} or \code{matrix(..., ncol=1)}
#' @param nrow   Number of rows, a single character representing rows symbolically, or an integer, generating
#'               that many rows.
#' @param ncol   Number of columns, a single character representing columns symbolically, or an integer, generating
#'               that many columns.
#' @param rownames optional vector of names for the matrix rows.
#'        if \code{symbol} is an R matrix with row names, these are used.
#'        For a matrix with a non-numeric (e.g., \code{"m"}) number of rows,
#'        3 names should be supplied, for the 1st, 2nd, and last rows.
#' @param colnames optional vector of names for the matrix columns.
#'        if \code{symbol} is an R matrix with column names, these are used.
#'        For a matrix with a non-numeric (e.g., \code{"n"}) number of columns,
#'        3 names should be supplied, for the 1st, 2nd, and last columns.

#' @param matrix Character string giving the LaTeX matrix environment used in \code{\\begin{}}, \code{\\end{}}. Typically one of:
#' \describe{
#'    \item{\code{"pmatrix"}}{uses parentheses: \code{"(", ")"}}
#'    \item{\code{"bmatrix"}}{uses square brackets: \code{"[", "]"}}
#'    \item{\code{"Bmatrix"}}{uses braces: \code{"{", "}"}}
#'    \item{\code{"vmatrix"}}{uses vertical bars: \code{"|", "|"}}
#'    \item{\code{"Vmatrix"}}{uses double vertical bars: \code{"||", "||"}}
#'    \item{\code{"matrix"}}{generates a plain matrix without delimeters}
#' }
#' The default is taken from the \code{"latexMatrixEnv"} option;
#' if this option isn't set, then \code{"pmatrix"} is used.
#'
#' @param diag   logical; if \code{TRUE}, off-diagonal elements are all 0 (and \code{nrow} must == \code{ncol})
#' @param sparse logical; if \code{TRUE} replace 0's with empty characters to print a sparse matrix
#' @param zero.based logical 2-vector; start the row and/or column indices at 0 rather than 1;
#'   the default is \code{c(FALSE, FALSE)}
#' @param end.at if row or column indices start at 0, should they end at \code{n - 1} and
#'   \code{m - 1} or at \code{n} and \code{m}? (where \code{n} and \code{m} represent the
#'   characters used to denote the number of rows and columns, respectively);
#'   the default is \code{c("n - 1", "m - 1")}; applies only when \code{nrow}
#'   or \code{ncol} are characters
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
#' @param prefix.row optional character string to be pre-pended to each matrix row index
#' @param prefix.col optional character string to be pre-pended to each matrix column index
#' @param onConsole if \code{TRUE}, the default, print the LaTeX code for
#'                  the matrix on the R console.
#'
#' @returns \code{latexMatrix()} returns an object of class \code{"latexMatrix"}
#'          which contains the LaTeX representation of the matrix as a character string,
#'          in the returned object are named:
#'          \itemize{
#'          \item \code{"matrix"} (the LaTeX representation of the matrix); 
#'          \item \code{"dim"} (\code{nrow} and \code{ncol}); 
#'          \item \code{"body"} (a character matrix of LaTeX expressions for the cells of the matrix);
#'          \item \code{"wrapper"}(the beginning and ending lines for the LaTeX matrix environment).
#'          }
#'          
#'          \code{partition()}, \code{rbind()}, \code{cbind()}, and indexing of
#'          \code{"latexMatrix"} objects also return a \code{"latexMatrix"} object.
#'
#' @author John Fox
#' @seealso \code{\link{latexMatrixOperations}}, \code{\link{matrix2latex}},
#'  \code{\link[clipr]{write_clip}}
#' @export
#' @examples
#' latexMatrix()
#'
#' # return value
#' mat <- latexMatrix()
#' str(mat)
#' cat(getLatex(mat))
#' 
#' # copy to clipboard (can't be done in non-interactive mode)
#' \dontrun{
#' clipr::write_clip(mat) 
#' } 
#'
#' # can use a complex symbol
#' latexMatrix("\\widehat{\\beta}", 2, 4)
#'
#' # numeric rows/cols
#' latexMatrix(ncol=3)
#' latexMatrix(nrow=4)
#' latexMatrix(nrow=4, ncol=4)
#'
#' # diagonal matrices
#' latexMatrix(nrow=3, ncol=3, diag=TRUE)
#' latexMatrix(nrow="n", ncol="n", diag=TRUE)
#' latexMatrix(nrow="n", ncol="n", diag=TRUE, sparse=TRUE)
#'
#' # commas, exponents, transpose
#' latexMatrix("\\beta", comma=TRUE, exponent="-1")
#' latexMatrix("\\beta", comma=TRUE, transpose=TRUE)
#' latexMatrix("\\beta", comma=TRUE, exponent="-1", transpose=TRUE)
#'
#' # for a row/column vector, wrap in matrix()
#' latexMatrix(matrix(LETTERS[1:4], nrow=1))
#' latexMatrix(matrix(LETTERS[1:4], ncol=1))
#'
#' # represent the SVD, X = U D V'  symbolically
#' X <- latexMatrix("x", "n", "p")
#' U <- latexMatrix("u", "n", "k")
#' D <- latexMatrix("\\lambda", "k", "k", diag=TRUE)
#' V <- latexMatrix("v", "k", "p", transpose = TRUE)
#' cat("\\mathrm{SVD:}\n", getLatex(X), "=\n", getLatex(U),
#'     getLatex(D), getLatex(V))
#'
#' # supply a matrix for 'symbol'
#' m <- matrix(c(
#'   "\\alpha", "\\beta",
#'   "\\gamma", "\\delta",
#'   "\\epsilon", "\\pi",
#'   0 , 0), 4, 2, byrow=TRUE)
#' latexMatrix(m)
#'
#' # Identity matrix
#' latexMatrix(diag(3))
#' latexMatrix(diag(3), sparse=TRUE)
#'
#' # prefix / suffix
#' latexMatrix(prefix="\\sqrt{", suffix="}")
#' latexMatrix(suffix="^{1/2}")
#'
#' # show size (order) of a matrix
#' latexMatrix(show.size=TRUE)
#' latexMatrix(nrow=3, ncol=4, show.size=TRUE)
#'
#' # handling fractions
#' m <- matrix(3/(1:9), 3, 3)
#' latexMatrix(m)
#' latexMatrix(m, digits=2)
#' latexMatrix(m, fractions=TRUE)
#'
#' # zero-based indexing
#' latexMatrix(zero.based=c(TRUE, TRUE))
#' 
#' # partitioned matrix
#' X <- latexMatrix(nrow=5, ncol=6)
#' partition(X, rows=c(2, 4), columns=c(3, 5))
#' 
#' # binding rows and columns; indexing
#' X <- latexMatrix("x", nrow=4, ncol=2)
#' Y <- latexMatrix("y", nrow=4, ncol=1)
#' Z <- latexMatrix(matrix(1:8, 4, 2))
#' cbind(X, Y, Z)
#' rbind(X, Z)
#' X[1:2, ]
#' X[-(1:2), ]
#' X[1:2, 2]
#' 
#' # defining row and column names
#' W <- latexMatrix(rownames=c("\\alpha_1", "\\alpha_2", "\\alpha_m"),
#'                  colnames=c("\\beta_1", "\\beta_2", "\\beta_n"))
#' W
#' Rownames(W) <- c("\\mathrm{Abe}", "\\mathrm{Barry}", "\\mathrm{Zelda}")
#' Colnames(W) <- c("\\mathrm{Age}", "\\mathrm{BMI}", "\\mathrm{Waist}")
#' W


latexMatrix <- function(
    symbol="x",
    nrow="n",
    ncol="m",
    rownames=NULL,
    colnames=NULL,
    matrix=getOption("latexMatrixEnv"),
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
    prefix.row="",
    prefix.col=""
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
  
  if (is.null(matrix)) matrix <- "pmatrix"
  
  end.at.n.minus.1 <- gsub(" ", "", end.at) == c("n-1", "m-1")
  
  if (isTRUE(transpose)) transpose <- "\\top"
  if (!missing(exponent) && !isFALSE(transpose)){
    exponent <- paste0("{", exponent, "^", transpose, "}")
    transpose <- FALSE
  }
  
  # start composing output string:
  
  result <- paste0(if (fractions) "\\renewcommand*{\\arraystretch}{1.5} \n",
                   # if (!missing(lhs)) paste0(lhs, " = \n"),
                   "\\begin{", matrix, "} \n"
  )
  
  # matrix input:
  
  if (is.matrix(symbol)){
    dimnames <- dimnames(symbol)
    if (!is.null(dimnames) && is.null(rownames) 
        && is.null(colnames)){
      rownames <- dimnames[[1]]
      colnames <- dimnames[[2]]
    }
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
                           if (nrow > 1)
                             if(prefix.row != '') paste0(prefix.row, '{', i - zero.based[1], '}')
                           else i - zero.based[1],
                           if (nrow > 1 && ncol > 1) comma,
                           if (ncol > 1)
                             if(prefix.col != '') paste0(prefix.col, '{', j - zero.based[2], '}')
                           else j - zero.based[2],
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
                       if (!isFALSE(transpose)) paste0("^", transpose),
                       "\n")
  
  x.mat <- strsplit(mat.result, "\\n")[[1]]
  pick <- c(1, length(x.mat))
  wrapper <- x.mat[pick] # LaTeX matrix environment
  body <- x.mat[-pick]
  body <- gsub('\\\\\\\\', '', body)
  splt <- sapply(body, function(x.mat) strsplit(x.mat, '&'))
  nrow.x <- length(splt)
  body <- unname(do.call(rbind, splt)) # matrix of LaTeX cells
  body <- sub(" *$", "", sub("^ *", "", body))
  if(sparse)
    mat.result <- gsub('[[:blank:]]+0[[:blank:]]+', ' ', mat.result)
  
  if (!is.null(rownames)){
    rownames <- as.character(rownames)
    if (!is.numeric(nrow)){
      if (length(rownames) != 3) 
        stop("there should be 3 row names")
      rownames <- c(rownames[1:2], "\\vdots", rownames[3])
    } else {
      if (length(rownames) != nrow) 
        stop("there should be ", nrow, " row names")
    }
  }
  
  if (!is.null(colnames)){
    colnames <- as.character(colnames)
    if (!is.numeric(ncol)){
      if (length(colnames) != 3) 
        stop("there should be 3 column names")
      colnames <- c(colnames[1:2], "\\cdots", colnames[3])
    } else {
      if (length(colnames) != ncol) 
        stop("there should be ", ncol, " column names")
    }
  }
  
  # "latexMatrix" object:
  
  result <- list(matrix = mat.result,
                 dim = c(nrow, ncol),
                 body = body,
                 wrapper = wrapper,
                 dimnames = list(rownames=rownames, colnames=colnames))
  class(result) <- 'latexMatrix'
  result
}

#' @rdname latexMatrix
#' @export
partition <- function(x, ...){
  UseMethod("partition")
}

#' @param rows row numbers \emph{after} which partition lines should
#'   be drawn in the LaTeX printed representation of the matrix;
#'   if omitted, then the matrix isn't partitioned by rows
#' @param columns column numbers \emph{after} which partition lines should
#'   be drawn in the LaTeX printed representation of the matrix;
#'   if omitted, then the matrix isn't partitioned by columns
#' @rdname latexMatrix
#' @export
partition.latexMatrix <- function(x, rows, columns, ...){
  wrapper <- getWrapper(x)
  wrapper[1] <- paste0(wrapper[1], " \n")
  matrix <- getBody(x)
  if (!missing(columns)){
    if (any(columns < 1 | columns > ncol(matrix) - 1)){
      stop("'columns' out of bounds")
    }
    cols <- rep("c", ncol(matrix))
    for (col in columns){
      cols[col] <- paste0(cols[col], " |")
    }
    wrapper[1] <- paste0(wrapper[1], 
                         "\\begin{array}{",
                         paste(cols, collapse = " "),
                         "}\n")
    wrapper[2] <- paste0("\\end{array}\n",
                         wrapper[2])
  }
  if (!missing(rows)){
    if (any(rows < 1 | rows > nrow(matrix) - 1)){
      stop("'rows' out of bounds")
    }
    for (row in rows){
      matrix[row + 1, 1] <- paste0("\\hline ", matrix[row + 1, 1])
    }
  }
  result <- wrapper[1]
  for (i in 1:nrow(matrix)){
    result <- paste0(result, 
                     paste(matrix[i, ], collapse=" & "),
                     "\\\\ \n")
  }
  result <- paste0(result, wrapper[2])
  x$matrix <- result
  x
}

# accessor functions:

#' @rdname latexMatrix
#' @export
getLatex <- function(x, ...){
  UseMethod("getLatex")
}

#' @rdname latexMatrix
#' @export
getLatex.latexMatrix <- function(x, ...){
  x$matrix
}

#' @rdname latexMatrix
#' @export
getBody <- function(x, ...){
  UseMethod("getBody")
}

#' @rdname latexMatrix
#' @export
getBody.latexMatrix <- function(x, ...){
  x$body
}

#' @rdname latexMatrix
#' @export
getWrapper <- function(x, ...){
  UseMethod("getWrapper")
}

#' @rdname latexMatrix
#' @export
getWrapper.latexMatrix <- function(x, ...){
  x$wrapper
}

#' @rdname latexMatrix
#' @export
Dim <- function(x, ...){
  UseMethod("Dim")
}

#' @rdname latexMatrix
#' @export
Dim.latexMatrix <- function(x, ...){
  x$dim
}

#' @rdname latexMatrix
#' @export
Nrow <- function(x, ...){
  UseMethod("Nrow")
}

#' @rdname latexMatrix
#' @export
Nrow.latexMatrix <- function(x, ...){
  (x$dim)[1L]
}

#' @rdname latexMatrix
#' @export
Ncol <- function(x, ...){
  UseMethod("Ncol")
}

#' @rdname latexMatrix
#' @export
Ncol.latexMatrix <- function(x, ...){
  (x$dim)[2L]
}

#' @param x a \code{"latexMatrix"} object
#' @param ...  for \code{rbind()} and \code{cbind()}, one or more 
#' \code{"latexMatrix"} objects with, respectively, the same number of
#' columns or rows;
#' otherwise, for compatibility with generic functions, may be ignored

# print() method:
#' @param cell.spacing a character whose width is used to try to even out spacing
#'        of printed cell elements; the default is taken from the \code{"cell.spacing"}
#'        option, and if that option isn't set the character \code{"e"} is used.
#' @param colname.spacing a character whose width is used to try to even out spacing
#'        of printed column names; the default is taken from the \code{"colname.spacing"}
#'        option, and if that option isn't set the character \code{"i"} is used.
#' @rdname latexMatrix
#' @export
print.latexMatrix <- function(x, onConsole=TRUE, 
                              cell.spacing=getOption("cell.spacing"),
                              colname.spacing=getOption("colname.spacing"),
                              ...){
  
  if (is.null(cell.spacing)) cell.spacing <- "e"
  if (is.null(colname.spacing)) colname.spacing <- "i"
  
  countChars <- function(string, adjust=TRUE){
    gsub("\\\\[[:alpha:]]*", if(adjust) "X" else "", string) |> 
      gsub("[_^{}]*", "", x = _) |>
      nchar()
  }
  
  if (!is.null(rownames(x)) || !is.null(colnames(x))){
    rownames <- rownames(x)
    colnames <- colnames(x)
    X <- getBody(x)
    
    # adjust size of column names
    max.col <- apply(countChars(X), 2, max)
    if (!is.null(colnames)){
      for (j in 1:length(colnames)){
        nchar <- if ("\\cdots" == colnames[j]) {
          3
        } else {
          countChars(colnames[j], adjust=FALSE) 
        }
        pad <- max(max.col[j] - nchar, 0) 
        colnames[j] <- paste0(colnames[j], 
                              paste0("\\phantom{", 
                                     paste(rep(colname.spacing, pad), collapse=""),
                                     "}"))
      }
    }
    if (!is.null(colnames)){
      nchar.colnames <- countChars(colnames, adjust=FALSE)
      for (i in 1:nrow(X)){
        for (j in 1:ncol(X)){
          xij <- X[i, j]
          nchar <- if ("\\cdots" == xij) {
            3
          } else if ("\\vdots" == xij) {
            NA
          } else if ("\\ddots" == xij) {
            NA
          } else {
            countChars(xij)
          }
          pad <- max(nchar.colnames[j] - nchar, 0)
          if (!is.na(pad) && pad > 0) {
            X[i, j] <- paste0("\\phantom{",
                              paste(rep(cell.spacing, pad), collapse=""), 
                              "}", xij)
          }
        }
      }
      XX <- latexMatrix(X, colnames=colnames, rownames=rownames)
      XX <- updateWrapper(XX, getWrapper(x))
      XX$dim <- Dim(x)
      x <- XX
    }
    
    latex <- getLatex(x)
    latex <- paste0("\\begin{matrix}\n",
                    if (!is.null(rownames))  "  & ",
                    if (!is.null(colnames)) paste0(" \\begin{matrix} \\phantom{", 
                                                   colname.spacing ,"} ", 
                                                   paste(colnames, collapse=" & "), 
                                                   "\n  \\end{matrix} \\\\ \n"),
                    if (!is.null(rownames)) paste0(" \\begin{matrix}  \n", 
                                                   paste(paste0("   ", 
                                                                rownames, "\\\\ \n"), 
                                                         collapse=""),
                                                   "\\end{matrix}  & \n"),
                    latex, "\\\\ \n",
                    "\\end{matrix} \n"
    )
    x$matrix <- latex
  }
  if (onConsole) cat(getLatex(x))
  invisible(x)
}

#' @param locals an optional list or named numeric vector of variables to be given
#'   specific numeric values; e.g.,
#'   \code{locals = list(a = 1, b = 5, c = -1, d = 4)} or
#'   \code{locals = c(a = 1, b = 5, c = -1, d = 4)}

#' @rdname latexMatrix
#' @export
is.numeric.latexMatrix <- function(x){
  x <- getBody(x)
  x <- suppressWarnings(as.numeric(x))
  !any(is.na(x)) 
}

#' @rdname latexMatrix
#' @export
as.double.latexMatrix <- function(x, locals=list(), ...){

  numericDimensions(x)

  if (!is.list(locals) && is.vector(locals) && is.numeric(locals)
      && !is.null(names(locals))) locals <- as.list(locals)

  X <- getBody(x)
  nrow <- nrow(X)
  X <- gsub(paste0("\\\\", getLatexMultSymbol()), "*", X)
  X <- gsub("\\}", ")",
            gsub("\\}\\{", ")/(",
                 gsub("\\\\[cdt]?frac\\{", "(", X)))
  warn <- options(warn = 2)
  on.exit(options(warn))
  X <- try(sapply(X, function(x) eval(parse(text=x), envir=locals)),
           silent=TRUE)
  if (inherits(X, "try-error")){
    stop("matrix cannot be coerced to 'double' ('numeric')")
  }

  matrix(X, nrow=nrow)
}

#' @param i row index or indices (negative indices to omit rows)
#' @param j column index or indices (negative indices to omit columns)
#' @param drop to match the generic indexing function, ignored
#' @rdname latexMatrix
#' @export
`[.latexMatrix` <- function(x, i, j, ..., drop){
  numericDimensions(x)
  X <- getBody(x)
  X <- X[i, j, drop=FALSE]
  X <- latexMatrix(X)
  updateWrapper(X, getWrapper(x))
}

#' @param deparse.level to match the generic \code{\link{rbind}()}
#' and \code{\link{cbind}()} functions; ignored
#' @rdname latexMatrix
#' @export
cbind.latexMatrix <- function(..., deparse.level){
  matrices <- list(...)
  nrow <- Nrow(matrices[[1]])
  if (length(matrices) == 1) return(matrices[[1]])
  else {
    X <- getBody(matrices[[1]])
    for (matrix in matrices[-1]){
      if (Nrow(matrix) != nrow) stop("number of rows not the same")
      Y <- getBody(matrix)
      X <- cbind(X, Y)
    }
    X <- latexMatrix(X)
    return(updateWrapper(X, getWrapper(matrices[[1]])))
  }
}

#' @rdname latexMatrix
#' @export
rbind.latexMatrix <- function(..., deparse.level){
  matrices <- list(...)
  ncol <- Ncol(matrices[[1]])
  if (length(matrices) == 1) return(matrices[[1]])
  else {
    X <- getBody(matrices[[1]])
    for (matrix in matrices[-1]){
      if (Ncol(matrix) != ncol) stop("number of columns not the same")
      Y <- getBody(matrix)
      X <- rbind(X, Y)
    }
    X <- latexMatrix(X)
    return(updateWrapper(X, getWrapper(matrices[[1]])))
  }
}

#' @rdname latexMatrix
#' @export
dimnames.latexMatrix <- function(x){
  x$dimnames
}

#' @param value for \code{"Dimnames<-()"}, a two-element list with,
#'        respectively, character vectors of row and column names;
#'        for  \code{"Rownames<-()"} and \code{"Colnames<-()"}, a
#'        vector of names.
#' @rdname latexMatrix
#' @export
`Dimnames<-` <- function (x, value) {
  UseMethod("Dimnames<-")
}

#' @rdname latexMatrix
#' @export
`Dimnames<-.latexMatrix` <- function (x, value) {
  oldnames <- dimnames(x)
  if (!is.list(value) || length(value) != 2)
    stop("'value' must be a 2-element list with row and column names")
  dim <- Dim(x)
  nrow <- dim[1]
  ncol <- dim[2]
  rownames <- value[[1]]
  colnames <- value[[2]]
  
  if (!is.null(rownames) && (length(rownames) > 1 || rownames != "")){
    if (!is.numeric(nrow)){
      if (length(rownames) != 3) 
        stop("there should be 3 row names")
      rownames <- c(rownames[1:2], "\\vdots", rownames[3])
    } else {
      if (length(rownames) != nrow) 
        stop("there should be ", nrow, " row names")
    }
  }
  
  if (!is.null(colnames) && (length(colnames) > 1 || colnames != "")){
    if (!is.numeric(ncol)){
      if (length(colnames) != 3) 
        stop("there should be 3 column names")
      colnames <- c(colnames[1:2], "\\cdots", colnames[3])
    } else {
      if (length(colnames) != ncol) 
        stop("there should be ", ncol, " column names")
    }
  }
  
  newnames <- oldnames
  if (!is.null(rownames) && (length(rownames) > 1 || rownames != "")) newnames$rownames <- rownames
  if (is.null(rownames)) newnames["rownames"] <- list(NULL)
  if (!is.null(colnames) && (length(colnames) > 1 || colnames != "")) newnames$colnames <- colnames
  if (is.null(colnames)) newnames["colnames"] <- list(NULL)
  x$dimnames <- newnames
  x
}

#' @rdname latexMatrix
#' @export
`Rownames<-` <- function(x, value){
  UseMethod("Rownames<-")
}

#' @rdname latexMatrix
#' @export
`Rownames<-.latexMatrix` <- function(x, value){
  names <- list(rownames=value, colnames="")
  Dimnames(x) <- names
  x
}

#' @rdname latexMatrix
#' @export
`Colnames<-` <- function(x, value){
  UseMethod("Colnames<-")
}

#' @rdname latexMatrix
#' @export
`Colnames<-.latexMatrix` <- function(x, value){
  names <- list(rownames="", colnames=value)
  Dimnames(x) <- names
  x
}

# unexported functions:

numericDimensions <- function(x){
  dim <- Dim(x)
  if (!is.numeric(dim)) stop ("'", deparse(substitute(x)),
                              "' does not have numeric dimensions")
  return(NULL)
}

parenthesize <- function(element){
  element <- if (grepl("[ +/^-]", element)) {
    paste0("(", element, ")")
  } else {
    element
  }
  element <- gsub("\\([+[:blank:]]*", "\\(", element)
  element <- gsub("[[:blank:]]*\\)", "\\)", element)
  element <- gsub("[[:blank:]]{2,}", " ", element)
  element
}

isOdd <- function(x){
  1 == x %% 2
}

updateWrapper <- function(result, wrapper){
  mat.env <- getOption("latexMatrixEnv")
  if (is.null(mat.env)) mat.env <- "pmatrix"
  matrix <- sub(paste0("begin\\{", mat.env, "\\}"), wrapper[1], 
                getLatex(result))
  matrix <- sub(paste0("end\\{", mat.env, "\\}"), wrapper[2], matrix)
  result$matrix <- matrix
  result$wrapper <- wrapper
  result
}

getLatexMultSymbol <- function(){
  latexMultSymbol <- getOption("latexMultSymbol")
  if (is.null(latexMultSymbol)) latexMultSymbol <- "\\cdot"
  latexMultSymbol
}
