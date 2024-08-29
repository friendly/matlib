print.latexMatrix <- function(x, onConsole=TRUE,  hline, vline, ...){
  if (onConsole) {
    if (missing(hline) && missing(vline)) cat(getLatex(x))
    matrix <- getLatex(x)
    wrapper <- getWrapper(x)
    if (!missing(hline)){
      for (line in hline){
        where <- gregexpr(" \\\\ ", matrix, fixed=TRUE)
        start <- where[[1]][line]
        matrix <- insertString(matrix, "\\hline", start + 4)
      }
    }
    if (!missing(vline)){
      cols <- rep("c", ncol(getBody(x)))
      for (line in vline){
        cols[line] <- paste0(cols[line], " |")
      }
      wrap1 <- paste0(wrapper[1], 
                           "\n\\begin{array}{",
                           paste(cols, collapse = " "),
                           "}")
      wrap2 <- paste0("\\end{array}\n",
                           wrapper[2])
    }
    matrix <- sub(wrapper[1], wrap1, matrix, fixed=TRUE)
    matrix <- sub(wrapper[2], wrap2, matrix, fixed=TRUE)
    cat(matrix)
  }
  invisible(x)
}

insertString <- function(string, insert, after){
  start <- substring(string, 1, after - 1)
  end <-substring(string, after)
  paste0(start, insert, end)
}

\if (FALSE){
  X <- latexMatrix(nrow=5, ncol= 6)
  print(X, hline=2, vline=2)
  print(X, hline=c(2, 4), vline=c(1, 3))
}
