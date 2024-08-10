# Decompose <- function(x){
#   x <- strsplit(x, "\\n")[[1]]
#   pick <- c(1, length(x))
#   wrapper <- x[pick]
#   body <- x[-pick]
#   body <- gsub('\\\\\\\\', '', body)
#   body <- gsub(' ', '', body)
#   splt <- sapply(body, function(x) strsplit(x, '&'))
#   nrow <- length(splt)
#   ncol <- length(splt[[1L]])
#   body <- unname(do.call(rbind, splt))
#   return(list(body=body, wrapper=wrapper))
# }

`+.symbolicMatrix` <- function(e1, e2){
  if (!inherits(e2, "symbolicMatrix")){
      stop(deparse(substitute(e2)),
           " is not of class 'symbolicMatrix'")
  }
  A <- getBody(e1)
  B <- getBody(e2)
  dimA <- dim(A)
  dimB <- dim(B)
  if(!all(dim(A) == dim(B))){
    stop('matricies are not conformable for addition')
  }
  wrapper <- getWrapper(e1)
  result <- matrix(paste(A, "+", B), dimA[1L], dimA[2L])
  result <- symbolicMatrix(result)
  matrix <- sub("\\\\begin\\{pmatrix\\}", wrapper[1], getMatrix(result))
  matrix <- sub("\\\\end\\{pmatrix\\}", wrapper[2], matrix)
  result$dim <- Dim(e1)
  result$matrix <- matrix
  result$wrapper <- wrapper
  result
}

t.symbolicMatrix <- function(x){
  result <- symbolicMatrix(t(getBody(x)))
  wrapper <- getWrapper(x)
  
  matrix <- sub("\\\\begin\\{pmatrix\\}", 
                wrapper[1], getMatrix(result))
  result$matrix <- sub("\\\\end\\{pmatrix\\}", wrapper[2], matrix)
  result$wrapper <- wrapper
  result$dim <- rev(Dim(x))
  result
}

if(FALSE) {
library(matlib)
matrix(c(1,3,0,1),2,2) |> symbolicMatrix(matrix="bmatrix") -> A
matrix(c(5,3,1,4),2,2) |> symbolicMatrix(matrix="bmatrix") -> B
C <- symbolicMatrix(nrow=2, ncol=2)
D <- symbolicMatrix()
A
B
C
D
Nrow(D)
Ncol(D)
Dim(D)
getBody(A)
getWrapper(A)
getMatrix(A)

A + B
A + C

# extractors
getBody(A + B)
getWrapper(A + B)
getMatrix(A + B)

getMatrix(A + B) |> cat()


# Eqn(A, " + ", B, " = ", A + B)
# # generates misplaced & when compiled
# 
# Eqn(A, " + ", C, " = ", A + C)
# # generates misplaced & wh, A + "foo")
# 
# Z <- symbolicMatrix(matrix(1:6, 3, 2), matrix="bmaen compiled
# 
# Eqn(A, " + ", D, " = ", A + D)
# Eqn(A, " + ", B, " = "trix")

Z <- symbolicMatrix(matrix(1:6, 3, 2))
Z
t(Z)

M <- symbolicMatrix(matrix="bmatrix")
M
t(M)
Dim(M)
Dim(t(M))
getBody(t(M))
getWrapper(t(M))
getMatrix(t(M))
}
