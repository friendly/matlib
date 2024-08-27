setOldClass("latexMatrix")

setMethod("kronecker", 
          signature(X = "latexMatrix", 
                    Y = "latexMatrix"), 
          function(X, Y, FUN, make.dimnames, ...) {
            
            numericDimensions(X)
            numericDimensions(Y)
            
            Xmat <- getBody(X)
            Ymat <- getBody(Y)
            
            Z <- .kronecker(Xmat, Ymat, 
                            function(x, y) {
                              x <- trimws(x)
                              y <- trimws(y)
                              zeros <- as.character(x) == "0" | 
                                as.character(y) == "0"
                              x <- sapply(x, parenthesize)
                              y <- sapply(y, parenthesize)
                              res <- paste0(x, " \\cdot ", y)
                              res[zeros] <- "0"
                              res
                            }
            )
            
            result <- latexMatrix(Z, ...)
            result <- updateWrapper(result, getWrapper(X))
            result
          }
)


if (FALSE){
  
  library(matlib)
  
  numericDimensions <- matlib:::numericDimensions
  parenthesize <- matlib:::parenthesize
  updateWrapper <- matlib:::updateWrapper
  
  X <- latexMatrix(matrix(1:6, 2, 3), matrix="bmatrix")
  Y <- latexMatrix(matrix(10*(1:6), 3, 2), matrix="bmatrix")
  a <- latexMatrix('a', ncol=1, nrow=1)
  I3 <- latexMatrix(diag(3))
  X
  Y
  a
  I3
  
  kronecker(a, X)
  kronecker(X, a)
  kronecker(X, Y)
  kronecker(I3, X)
  kronecker(I3, X, sparse = TRUE)
  
  I3 %x% X # doesn't work because of conflicting definitions of
           #   kronecker() in base and methods packages
  a %x% I3 # doesn't work
  
  `%X%` <- function(x, y) kronecker(x, y)
  
  X %X% Y
  
  A <- latexMatrix("a", 2, 2)
  B <- latexMatrix("b", 2, 2)
  kronecker(A, B)
  
  # How could I generate the 'definition' of Kronecker product,
  A <- latexMatrix('a', nrow=2, ncol=2)
  B <- latexMatrix('\\mathbf{B}', ncol=1, nrow=1)
  kronecker(A, B)
  
}
