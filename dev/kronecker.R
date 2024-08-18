setOldClass("latexMatrix")

setMethod("kronecker", 
          signature(X = "latexMatrix", 
                    Y = "latexMatrix"), 
          function(X, Y, FUN, make.dimnames, ...) {
            
            numericDimensions(X)
            numericDimensions(Y)
            
            dimX <- Dim(X)
            Xmat <- getBody(X)
            wrapper <- getWrapper(X)
            lst.row <- vector('list', dimX[1])
            dimY <- Dim(Y)
            zero.Y <- matrix('0', nrow = dimY[1], ncol = dimY[2])
            Ymat <- getBody(Y)
            zero.Y.ind <- Ymat == '0'
            
            for (i in seq_len(dimX[1])) {
              lst <- vector('list', dimX[2])
              for (j in seq_len(dimX[2])) {
                e <- latexMatrix(Xmat[i, j], nrow = 1, ncol = 1)
                e.body <- getBody(e)[1, 1]
                e.convert <- type.convert(e.body, as.is = TRUE)
                lst[[j]] <- if (is.numeric(e.convert) &&
                                isTRUE(all.equal(e.convert, 0))) {
                  zero.Y
                } else {
                  out <- matrix(paste0(e.body, ' \\cdot ', Ymat),
                                nrow = dimY[1],
                                ncol = dimY[2])
                  out[zero.Y.ind] <- '0'
                  out
                }
              }
              mats <- lapply(lst, function(x)
                if (is(x, 'latexMatrix'))
                  getBody(x)
                else
                  x)
              lst.row[[i]] <- do.call(cbind, mats)
            }
            
            Z <- do.call(rbind, lst.row)
            result <- latexMatrix(Z, ...)
            matrix <- sub("begin\\{pmatrix\\}", wrapper[1], getLatex(result))
            matrix <- sub("end\\{pmatrix\\}", wrapper[2], matrix)
            result$matrix <- matrix
            result
          }
)

if (FALSE){
  
  library(matlib)
  
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
  
  I3 %x% X # doesn't work
  a %x% I3 # doesn't work
  
  `%X%` <- function(x, y) kronecker(x, y)
  
  X %xx% Y
  
}
