symbolicMatrix <- function(
    symbol="x",
    nrow="n",
    ncol="m",
    matrix="pmatrix",
    diag=FALSE,
    zero.based=c(FALSE, FALSE),
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
  
  if (isTRUE(transpose)) transpose <- "\\top"
  if (!missing(exponent) && !isFALSE(transpose)){
    exponent <- paste0("{", exponent, "^", transpose, "}")
    transpose <- FALSE
  }
  
  result <- paste0(if (fractions) "\\renewcommand*{\\arraystretch}{1.5} \n",
                   if (!missing(lhs)) paste0(lhs, " = \n"),
                   "\\begin{", matrix, "} \n"
  )
  
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
    
    row.elements <- c(symbol, symbol, "\\cdots", symbol)
    col.subscripts <- if (!zero.based[2]) {
      c("1", "2", "", ncol)
      } else {
        c("0", "1", "", if (is.numeric(ncol)) ncol - 1 else paste0(ncol, " - 1"))
      }
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
        diag(mat) <- paste0(prefix, symbol, "_{", (!zero.based[1]):(nrow - zero.based[1]), "}", suffix)
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
                     c("_{0}", "_{1}", paste0("_{", nrow, " - 1", "}"))
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
      
      vdots <- paste0("\\vdots",
                      paste0(paste(rep(" ",
                                       nchar(symbol) + nchar(prefix) + nchar(suffix) - 1),
                                   collapse = ""),
                             if (comma == ",") " "))
      row.subscripts <- if (!zero.based[1]){
        c("1", "2", "", nrow)
      } else {
        c("0", "1", "", paste0(nrow, " - 1"))
      }
      
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
                                    if (ncol > 1) paste0(comma, j - zero.based[2]), "}",
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
                           if (j != 3) paste0(i - zero.based[1], comma),
                           col.subscripts[j],
                           right.sub[j],  if (j != 3) suffix,
                           post.element[j])
        }
      }
      
    } else {
      for (i in 1:nrow){
        result <- paste0(result, "  ")
        for (j in 1:ncol){
          result <- paste0(result, prefix, symbol, "_{", if (nrow > 1) i - zero.based[1],
                           if (nrow > 1 && ncol > 1) comma,
                           if (ncol > 1) j - zero.based[2], "}", suffix,
                           if (j == ncol) " \\\\ \n" else " & ")
        }
      }
    }
  }
  
  mat.result <- paste0(result, "\\end{", matrix, "}",
                   if (show.size) paste0("_{(", nrow, " \\times ", ncol, ")}" ),
                   if (!missing(exponent)) paste0("^{", exponent, "}"),
                   if (!isFALSE(transpose)) paste0("^", transpose),
                   "\n")
  
  x.mat <- strsplit(mat.result, "\\n")[[1]]
  pick <- c(1, length(x.mat))
  wrapper <- x.mat[pick]
  body <- x.mat[-pick]
  body <- gsub('\\\\\\\\', '', body)
  body <- gsub(' ', '', body)
  splt <- sapply(body, function(x.mat) strsplit(x.mat, '&'))
  nrow.x <- length(splt)
  # ncol.x <- length(splt[[1L]])
  body <- unname(do.call(rbind, splt))
  
  result <- list(matrix = mat.result,
                 dim = c(nrow, ncol),
                 body = body,
                 wrapper = wrapper)
  class(result) <- 'symbolicMatrix'
  result
}

getMatrix <- function(x, ...){
  UseMethod("getMatrix")
}

getMatrix.symbolicMatrix <- function(x, ...){
  x$matrix
}

getBody <- function(x, ...){
  UseMethod("getBody")
}

getBody.symbolicMatrix <- function(x, ...){
  x$body
}

getWrapper <- function(x, ...){
  UseMethod("getWrapper")
}
getWrapper.symbolicMatrix <- function(x, ...){
  x$wrapper
}

print.symbolicMatrix <- function(x, onConsole=TRUE,  ...){
  if (onConsole) cat(getMatrix(x))
  invisible(x)
}

Dim <- function(x, ...){
  UseMethod("Dim")
}
Dim.symbolicMatrix <- function(x, ...){
  x$dim
}

Nrow <- function(x, ...){
  UseMethod("Nrow")
}
Nrow.symbolicMatrix <- function(x, ...){
  (x$dim)[1L]
}

Ncol <- function(x, ...){
  UseMethod("Ncol")
}
Ncol.symbolicMatrix <- function(x, ...){
  (x$dim)[2L]
}
