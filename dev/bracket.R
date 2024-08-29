`[.latexMatrix` <- function(x, i, j, ..., drop){
  matlib:::numericDimensions(x)
  X <- getBody(x)
  X <- X[i, j, drop=FALSE]
  X <- latexMatrix(X)
  matlib:::updateWrapper(X, getWrapper(x))
}

cbind.latexMatrix <- function(..., deparse.level = 1){
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
    return(matlib:::updateWrapper(X, getWrapper(matrices[[1]])))
  }
}

rbind.latexMatrix <- function(..., deparse.level = 1){
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
    return(matlib:::updateWrapper(X, getWrapper(matrices[[1]])))
  }
}

if (FALSE){
  X <- latexMatrix("x", nrow=4, ncol=2)
  Y <- latexMatrix("y", nrow=4, ncol=1)
  Z <- latexMatrix(matrix(1:8, 4, 2))
  cbind(X)
  cbind(X, Y)
  cbind(X, Y, Z)
  rbind(X, Z)
  X[1:2, ]
  X[-(1:2), ]
  X[1:2, 2]
}
