matsum <- function(A, ...){
  UseMethod("matsum")
}

`+.latexMatrix` <- function(e1, e2){
  matsum(e1, e2)
}

matsum.latexMatrix <- function(A, ..., as.numeric=TRUE){
  
  matrices <- list(...)
  
  if (any(sapply(matrices, function(x) !inherits(x, "latexMatrix")))){
    stop("arguments are not all of class 'latexMatrix'")
  }
  
  matlib:::numericDimensions(A)
  
  dimnames <- dimnames(A)
  for (M in matrices)   {
    matlib:::numericDimensions(M)
    if (!isTRUE(all.equal(dimnames, dimnames(M)))){
      stop("matrix dimension names don't match")
    }
  }
  
  wrapper <- getWrapper(A)
  
  if (as.numeric && is.numeric(A) && all(sapply(matrices, is.numeric))){
    A <- as.numeric(A)
    dimA <- dim(A)
    matrices <- lapply(matrices, as.numeric)
    for (i in seq_along(matrices)){
      if (!all(dim(matrices[[i]]) == dimA)) 
        stop ("the matrices are not conformable for addition")
      A <- A + matrices[[i]]
    }
  } else {
    A <- getBody(A)
    dimA <- dim(A)
    for (M in matrices){
      M <- getBody(M)
      if(!all(dim(A) == dim(M)))
        stop('matricies are not conformable for addition')
      A <- matrix(paste(sapply(A, matlib:::parenthesize), "+", 
                        sapply(M, matlib:::parenthesize)), 
                  dimA[1L], dimA[2L])
    }
  }
  A <- latexMatrix(A)
  A <- matlib:::updateWrapper(A, wrapper)
  Dimnames(A) <- dimnames
  A
}

if (FALSE) {
  A <- latexMatrix("a", 3, 3, rownames=letters[1:3], colnames=LETTERS[1:3])
  B <- latexMatrix("b", 3, 3, rownames=letters[1:3], colnames=LETTERS[1:3])
  C <- latexMatrix("c", 3, 3, rownames=letters[4:6], colnames=LETTERS[4:6])
  D <- latexMatrix("d", 3, 3)
  A
  B
  C
  D
  A + B
  A + C
  A + D
}
