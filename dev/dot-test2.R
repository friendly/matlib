# Now handle numeric X, Y by direct matrix multiplication

numericDimensions <- matlib:::numericDimensions
updateWrapper <- matlib:::updateWrapper
parenthesize <- matlib:::parenthesize
getLatexMultSymbol <- matlib:::getLatexMultSymbol

source(here::here("dev", "dot.R"))

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
  
  XX <- suppressWarnings(as.numeric(X))
  YY <- suppressWarnings(as.numeric(Y))
  if (!any(is.na(XX)) && !any(is.na(YY))){
    XX <- matrix(XX, dimX[1], dimX[2])
    YY <- matrix(YY, dimY[1], dimY[2])
    Z <- XX %*% YY
    
  } else {
    
    Z <- matrix("", nrow(X), ncol(Y))
    for (i in 1:nrow(X)){
      for (j in 1:ncol(Y)){
        for (k in 1:ncol(X)){
          Z[i, j] <- dot(X[i, ], Y[, j])
        }
      }
    }
  }
  
  result <- latexMatrix(Z)
  result <- updateWrapper(result, getWrapper(x))
  result$dim <- dim(Z)
  result
}

dot <- function(x, y) {
  if (length(x) != length(y)) stop("Vectors must have the same length")
  x <- trimws(x)
  y <- trimws(y)
  res <- ""
  for (i in 1:length(x)) {
    # ignore terms multiplied by zero
    if (x[i] == "0") next
    if (y[i] == "0") next
    xi <- if(x[i] == "1") "" else x[i]
    yi <- if(y[i] == "1") "" else y[i]
    if (x[i] == "1" && y[i] == "1") xi <- "1"
    xi <- if (xi == "-1") "-" else xi
    if (y[i] == "-1") {
      yi <- ""
      xi <- if (x[i] == "-1") "1" else paste0("-", parenthesize(xi))
    }
    times <- if(xi == "" || xi == "-" || yi == "") "" else " \\cdot "
    res <- paste0(res,
                  if (nchar(res) > 0) " + ",
                  if (y[i] != "-1" && xi != "-") parenthesize(xi) else xi,
                  times,
                  parenthesize(yi))
  }
  if (res == "") res <- "0"
  if (res == "-") res <- "-1"
  res
}

if (FALSE) {
  
(A <- latexMatrix(matrix(c(1, -3, 0, 1), 2, 2)))
(B <- latexMatrix(matrix(c(5, 3, -1, 4), 2, 2)))
(C <- latexMatrix(symbol="c", 2, 2))
(D <- latexMatrix(symbol="d", 2, 2))
  
A %*% B
A %*% C
B %*% C
C %*% D

A %*% (B - C)
A %*% -B
(A - 2*D) %*% B

  
C <- latexMatrix(matrix(c(0, 1, 0, 0,
                          0, 0, 1, 0), nrow=2, byrow=TRUE), 
                 matrix = "bmatrix")
B <- latexMatrix('\\beta', ncol = 3, nrow=4, 
                 comma=TRUE, prefix.col = 'y_',
                 zero.based=c(TRUE, FALSE))
C %*% B


}
