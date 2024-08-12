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
  matrix <- sub("begin\\{pmatrix\\}", wrapper[1], getLatex(result))
  matrix <- sub("end\\{pmatrix\\}", wrapper[2], matrix)
  result$dim <- Dim(e1)
  result$matrix <- matrix
  result$wrapper <- wrapper
  result
}

`-.symbolicMatrix` <- function(e1, e2){
  if (!inherits(e2, "symbolicMatrix")){
    stop(deparse(substitute(e2)),
         " is not of class 'symbolicMatrix'")
  }
  A <- getBody(e1)
  B <- getBody(e2)
  dimA <- dim(A)
  dimB <- dim(B)
  if(!all(dim(A) == dim(B))){
    stop('matricies are not conformable for subtraction')
  }
  wrapper <- getWrapper(e1)
  result <- matrix(paste(A, "-", B), dimA[1L], dimA[2L])
  result <- symbolicMatrix(result)
  matrix <- sub("begin\\{pmatrix\\}", wrapper[1], getLatex(result))
  matrix <- sub("end\\{pmatrix\\}", wrapper[2], matrix)
  result$dim <- Dim(e1)
  result$matrix <- matrix
  result$wrapper <- wrapper
  result
}

t.symbolicMatrix <- function(x){
  result <- symbolicMatrix(t(getBody(x)))
  wrapper <- getWrapper(x)

  matrix <- sub("begin\\{pmatrix\\}",
                wrapper[1], getLatex(result))
  result$matrix <- sub("end\\{pmatrix\\}", wrapper[2], matrix)
  result$wrapper <- wrapper
  result$dim <- rev(Dim(x))
  result
}

parenthesize <- function(element){
  if (grepl("[ +-/^]", element)) {
    paste0("(", element, ")")
  } else {
    element
  }

}

`%*%.symbolicMatrix` <- function(x, y){
  if (!inherits(y, "symbolicMatrix")){
    stop(deparse(substitute(y)),
         " is not of class 'symbolicMatrix'")
  }
  X <- getBody(x)
  Y <- getBody(y)
  dimX <- dim(X)
  dimY <- dim(Y)
  if (dimX[2] != dimY[1]){
    stop('matricies are not conformable for multiplication')
  }
  wrapper <- getWrapper(x)

  Z <- matrix("", nrow(X), ncol(Y))
  for (i in 1:nrow(X)){
    for (j in 1:ncol(Y)){
      for (k in 1:ncol(X)){
        Z[i, j] <- paste0(Z[i, j],
                          if (k > 1) " + ",
                          parenthesize(X[i, k]),
                          " \\times ",
                          parenthesize(Y[k, j]))
      }
    }
  }
  result <- symbolicMatrix(Z)
  matrix <- sub("begin\\{pmatrix\\}", wrapper[1], getLatex(result))
  matrix <- sub("end\\{pmatrix\\}", wrapper[2], matrix)
  result$dim <- dim(Z)
  result$matrix <- matrix
  result$wrapper <- wrapper
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
getLatex(A)
Dim(A)

A + B
A + C

# extractors
getBody(A + B)
getWrapper(A + B)
getLatex(A + B)

getLatex(A + B) |> cat()

cat(getLatex(A), " +\\large\n", getLatex(B), "\\quad\\large=\\quad\n", getLatex(A + B))

# or keeping the numeric version
A <-matrix(c(1,3,0,1),2,2)
getLatex(symbolicMatrix(A))
B <- matrix(c(5,3,1,4),2,2)
getLatex(symbolicMatrix(B))
getLatex(symbolicMatrix(A + B))


# Eqn(A, " + ", B, " = ", A + B)
# # generates misplaced & when compiled
# Eqn(A, " + ", D, " = ", A + D)
# Eqn(A, " + ", B, " = ", A + "foo")
#
# Z <- symbolicMatrix(matrix(1:6, 3, 2), matrix="bmatrix")
#
# Eqn(A, " + ", D, " = ", A + D)
# Eqn(A, " + ", B, " = ", A + B)

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
getLatex(t(M))

X <- symbolicMatrix(matrix(letters[1:6], 2, 3))
Y <- symbolicMatrix(matrix(LETTERS[1:6], 3, 2))
X
Y
X %*% Y

W <- symbolicMatrix(matrix(letters[7:12], 2, 3))
X + W
(X + W) %*% Y

X <- symbolicMatrix(matrix(1:6, 2, 3), matrix="bmatrix")
Y <- symbolicMatrix(matrix(10*(1:6), 3, 2), matrix="bmatrix")
X
Y
X %*% Y

}
