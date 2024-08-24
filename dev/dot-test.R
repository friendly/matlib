# Test using dot() for matrix mult

numericDimensions <- matlib:::numericDimensions
updateWrapper <- matlib:::updateWrapper
parenthesize <- matlib:::parenthesize

`%*%.latexMatrix` <- function(x, y){
  if (!inherits(y, "latexMatrix")){
    stop(deparse(substitute(y)),
         " is not of class 'latexMatrix'")
  }
  numericDimensions(x)
  numericDimensions(y)
  X <- getBody(x)
  Y <- getBody(y)
  dimX <- dim(X)
  dimY <- dim(Y)
  if (dimX[2] != dimY[1]){
    stop('matricies are not conformable for multiplication')
  }
  
  Z <- matrix("", nrow(X), ncol(Y))
  for (i in 1:nrow(X)){
    for (j in 1:ncol(Y)){
      for (k in 1:ncol(X)){
        Z[i, j] <- dot(X[i, ], Y[, j])
      }
    }
  }
  result <- latexMatrix(Z)
  result <- updateWrapper(result, getWrapper(x))
  result$dim <- dim(Z)
  result
}

if(FALSE) {
  (A <- latexMatrix(matrix(c(1, -3, 0, 1), 2, 2)))
  (B <- latexMatrix(matrix(c(5, 3, -1, 4), 2, 2)))
  (C <- latexMatrix(symbol="c", 2, 2))
  (D <- latexMatrix(symbol="d", 2, 2))

  A %*% B
  
  as.double(A)
  as.double(B)
  as.double(A) %*% as.double(B)
  
  A %*% C
  A %*% (B - C)
  A %*% -B
  (A - 2*D) %*% B
  
  A %*% solve(B)
}