partition <- function(x, rows, columns){
  wrapper <- getWrapper(x)
  wrapper[1] <- paste0(wrapper[1], " \n")
  matrix <- getBody(x)
  if (!missing(columns)){
    if (any(columns < 1 | columns > ncol(matrix))){
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
    if (any(rows < 1 | rows > nrow(matrix))){
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

if (FALSE){
  X <- latexMatrix(nrow=5, ncol=6)
  partition(X)
  partition(X, rows=3)
  partition(X, columns=2)
  partition(X, rows=c(2, 4), columns=c(3, 5))
}
