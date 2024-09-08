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


dimnames.latexMatrix <- function(x){
  x$dimnames
}

print.latexMatrix <- function(x, onConsole=TRUE,  ...){
  
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
                              paste(rep("~", pad), collapse=""))
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
                              paste(rep("e", pad), collapse=""), 
                              "}", xij)
          }
        }
      }
      XX <- latexMatrix(X, colnames=colnames, rownames=rownames)
      XX <- matlib:::updateWrapper(XX, getWrapper(x))
      XX$dim <- Dim(x)
      x <- XX
    }
    
    latex <- getLatex(x)
    latex <- paste0("\\begin{matrix}\n",
                    if (!is.null(rownames))  "  & ",
                    if (!is.null(colnames)) paste0(" \\begin{matrix} ", 
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

`Dimnames<-` <- function (x, value) {
  UseMethod("Dimnames<-")
}

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

`Rownames<-` <- function(x, value){
  UseMethod("Rownames<-")
}

`Rownames<-.latexMatrix` <- function(x, value){
  names <- list(rownames=value, colnames="")
  Dimnames(x) <- names
  x
}

`Colnames<-` <- function(x, value){
  UseMethod("Colnames<-")
}

`Colnames<-.latexMatrix` <- function(x, value){
  names <- list(rownames="", colnames=value)
  Dimnames(x) <- names
  x
}

if (FALSE){
  
A <- latexMatrix("a", nrow=2, ncol=3, rownames=letters[1:2], 
                 colnames=LETTERS[1:3])
A
dimnames(A)
rownames(A)
colnames(A)

B <- latexMatrix(nrow=2, ncol=3, colnames=LETTERS[1:3])
dimnames(B)
rownames(B)
colnames(B)
B

D <- latexMatrix(nrow=2, ncol=3, rownames=letters[1:2])
dimnames(D)
D

X <- latexMatrix(rownames=letters[1:3], colnames=LETTERS[1:3])
dimnames(X)
rownames(X)
colnames(X)
X

C <- latexMatrix(matrix(letters[1:24], 3, 8), rownames=letters[1:3],
                 colnames=LETTERS[1:8])
C # alignment of column names breaks down

G <- latexMatrix(rownames=c("\\alpha", "\\beta", "\\omega"),
                 colnames=c("A", "B", "\\Omega"))
G

W <- latexMatrix(rownames=c("\\alpha_1", "\\alpha_2", "\\alpha_m"),
                 colnames=c("\\beta_1", "\\beta_2", "\\beta_n"))
W

Z <- latexMatrix(prefix="\\sqrt{", suffix="}",
                 rownames=c("\\alpha_1", "\\alpha_2", "\\alpha_m"),
                 colnames=c("\\beta_1", "\\beta_2", "\\beta_n"))
Z # alignment of column names breaks down

AA <- A
dimnames(AA)
Dimnames(AA) <- list(letters[3:4], LETTERS[4:6])
dimnames(AA)

Rownames(AA) <- letters[10:11]
dimnames(AA)

Rownames(AA) <- NULL
dimnames(AA)

Colnames(AA) <- LETTERS[10:12]
dimnames(AA)

mat <- matrix(sample(25), nrow = 5,
              dimnames = list(who = c("Abe", "Bart", "Cat", "Doug", "Eve"),
                              what = c("Geog", "Hist", "Math", "Read", "Spell")))

latexMatrix(mat)

latexMatrix(mat, matrix="bmatrix",
            rownames = paste0("\\mathrm{", rownames(mat), "}"), 
            colnames = paste0("\\mathrm{", colnames(mat), "}"))

latexMatrix(mat, 
            rownames = paste0("\\mathrm{", rownames(mat), "}"), 
            colnames = paste0("\\mathrm{", colnames(mat), "}"))

M <- latexMatrix(matrix="bmatrix", rownames=letters[1:3], 
            colnames=LETTERS[1:3])
M
}
