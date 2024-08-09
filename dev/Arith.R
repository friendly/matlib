Decompose <- function(x){
  x <- strsplit(x, "\\n")[[1]]
  pick <- c(1, length(x))
  wrapper <- x[pick]
  body <- x[-pick]
  body <- gsub('\\\\\\\\', '', body)
  body <- gsub(' ', '', body)
  splt <- sapply(body, function(x) strsplit(x, '&'))
  nrow <- length(splt)
  ncol <- length(splt[[1L]])
  body <- unname(do.call(rbind, splt))
  return(list(body=body, wrapper=wrapper))
}

`+.symbolicMatrix` <- function(e1, e2){
  if (!inherits(e2, "symbolicMatrix")){
      stop(deparse(substitute(e2)),
           " is not of class 'symbolicMatrix'")
  }
  d.e1 <- Decompose(e1)
  d.e2 <- Decompose(e2)
  A <- d.e1$body
  B <- d.e2$body
  dimA <- dim(A)
  dimB <- dim(B)
  if(!all(dim(A) == dim(B))){
    stop('matricies are not conformable for addition')
  }
  wrapper <- d.e1$wrapper
  result <- matrix(paste(A, "+", B), dimA[1L], dimA[2L])
  result <- symbolicMatrix(result)
  result <- sub("\\\\begin\\{pmatrix\\}", wrapper[1], result)
  result <- sub("\\\\end\\{pmatrix\\}", wrapper[2], result)
  result
}

t.symbolicMatrix <- function(x){
  d.x <- Decompose(x)
  result <- symbolicMatrix(t(d.x$body))
  wrapper <- d.x$wrapper
  result <- sub("\\\\begin\\{pmatrix\\}", wrapper[1], result)
  result <- sub("\\\\end\\{pmatrix\\}", wrapper[2], result)
  result
}

library(matlib)
matrix(c(1,3,0,1),2,2) |> symbolicMatrix(matrix="bmatrix") -> A
matrix(c(5,3,1,4),2,2) |> symbolicMatrix(matrix="bmatrix") -> B
C <- symbolicMatrix(nrow=2, ncol=2)
D <- symbolicMatrix()

A + B
A + C


Eqn(A, " + ", B, " = ", A + B)
Eqn(A, " + ", C, " = ", A + C)
Eqn(A, " + ", D, " = ", A + D)
Eqn(A, " + ", B, " = ", A + "foo")

Z <- symbolicMatrix(matrix(1:6, 3, 2), matrix="bmatrix")
Z
t(Z)
