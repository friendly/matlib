symbolicMatrix <- function(symbol="x", nrow="m", ncol="n", matrix="pmatrix",
                           diag=FALSE){
  
  # Args:
  #   symbol: for matrix elements, character string
  #   nrow: number of rows, can be a character
  #   ncol: number of columns, can be a character
  #   matrix: LaTeX matix environment
  #   diag: if TRUE, off-diagonal elements are all 0 (and nrow must == ncol)
  
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
                           if (j !=3) row.subscripts[i], 
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
                           paste0(symbol, "_{", row.subscripts[i], if (ncol > 1) j, "}",
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
                           if (j !=3 && nrow > 1) i, 
                           col.subscripts[j],
                           right.sub[j], post.element[j])
        }
      }
    } else {
      for (i in 1:nrow){
        result <- paste0(result, "  ")
        for (j in 1:ncol){
          result <- paste0(result, symbol, "_{", if (nrow > 1) i, 
                           if (ncol > 1) j, "}",
                           if (j == ncol) " \\\\ \n" else " & ")
        }
      }
    }
  result <- paste0(result, "\\end{", matrix, "} \n")
  cat(result)
  invisible(result)
}
