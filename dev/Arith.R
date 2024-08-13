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

numericDimensions <- function(x){
  UseMethod("numericDimensions")
}

numericDimensions.symbolicMatrix <- function(x){
  dim <- Dim(x)
  if (!is.numeric(dim)) stop ("'", deparse(substitute(x)),
                              "' does not have numeric dimensions")
  return(NULL)
}

`+.symbolicMatrix` <- function(e1, e2){
  if (!inherits(e2, "symbolicMatrix")){
      stop(deparse(substitute(e2)),
           " is not of class 'symbolicMatrix'")
  }
  numericDimensions(e1)
  numericDimensions(e2)
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
  numericDimensions(e1)
  numericDimensions(e2)
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
  numericDimensions(x)
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
  numericDimensions(x)
  numericDimensions(y)
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
                          " \\cdot ",
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

`*.symbolicMatrix` <- function(x, y){
    if (!inherits(y, "symbolicMatrix")){
        stop(deparse(substitute(y)),
             " is not of class 'symbolicMatrix'")
    }
    numericDimensions(x)
    numericDimensions(y)
    X <- getBody(x)
    Y <- getBody(y)
    dimX <- dim(X)
    dimY <- dim(Y)
    if(is.numeric(dimX) && prod(dimX) == 1L){
        tmp <- symbolicMatrix(X[1L,1L], nrow=dimY[1L], ncol=dimY[2L])
        X <- getBody(tmp)
        X <- gsub("_.*", "", X)
        dimX <- dimY
    }
    if(is.numeric(dimY) && prod(dimY) == 1L){
        tmp <- symbolicMatrix(Y[1L,1L], nrow=dimX[1L], ncol=dimX[2L])
        Y <- getBody(tmp)
        Y <- gsub("_.*", "", Y)
        dimY <- dimX
    }
    if (!all(dimX == dimY)){
        stop('matricies are not conformable for element-wise multiplication')
    }
    wrapper <- getWrapper(x)
    Z <- matrix("", nrow(X), ncol(X))
    for (i in 1:nrow(X)){
        for (j in 1:ncol(Y)){
            Z[i, j] <- paste0(parenthesize(X[i, j]),
                              " \\cdot ", parenthesize(Y[i, j]))
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

isOdd <- function(x){
  1 == x %% 2
}

determinant.symbolicMatrix <- function(x, logarithm, ...){

  # determinant by minors and cofactors

  # helper function for recursion:
  DET <- function(X){
    if (nrow(X) == 2){
      paste0(X[1, 1], " \\cdot ", X[2, 2], " - ",
             X[1, 2], " \\cdot ", X[2, 1])
    } else {
      indices <- 1:ncol(X)
      res <- ""
      for (j in indices){
        res <- paste0(res, if (isOdd(j)) " + " else " - ",
                      X[1, j], " \\cdot ",
                      parenthesize(DET(X[-1, indices != j]))
        )
      }
      res
    }
  }

  numericDimensions(x)

  sub("^[ +]*", "", DET(getBody(x)))
}

as.double.symbolicMatrix <- function(x, locals=list(), ...){

  numericDimensions(x)

  X <- getBody(x)
  nrow <- nrow(X)
  X <- gsub("\\\\cdot", "\\*", X)

  warn <- options(warn = 2)
  on.exit(options(warn))
  X <- try(sapply(X, function(x) eval(parse(text=x), envir=locals)),
           silent=TRUE)
  if (inherits(X, "try-error")){
    stop("matrix cannot be coerced to 'double' ('numeric')")
  }

  matrix(X, nrow=nrow)
}

# symbolic matrix inverse:

solve.symbolicMatrix <- function (a, b, simplify=FALSE, ...) {

  # b: ignored
  # simplify: if TRUE return LaTeX expression with 1/det as multiplier

  numericDimensions(a)
  if (Nrow(a) != Ncol(a)) stop("matrix 'a' must be square")
  if (!missing(b)) warning("'b' argument to solve() ignored")

  wrapper <- getWrapper(a)

  det <- parenthesize(determinant(a))
  A <- getBody(a)
  n <- nrow(A)
  indices <- 1:n
  A_inv <- matrix("", n, n)

  for (i in 1:n){
    for (j in 1:n){
      A_ij <- symbolicMatrix(A[indices[-i], indices[-j], drop=FALSE])
      A_inv[i, j] <- if (Nrow(A_ij) == 1) { # cofactors
        A[indices[-i], indices[-j]]
      } else{
        determinant(A_ij)
      }
      if (isOdd(i + j)) A_inv[i, j] <- paste0("-", parenthesize(A_inv[i, j]))
      if (!simplify) A_inv[i, j] <- paste0(parenthesize(A_inv[i, j]), "/", det)
    }
  }

  A_inv <- t(A_inv) # adjoint
  result <- symbolicMatrix(A_inv)
  matrix <- sub("begin\\{pmatrix\\}",
                wrapper[1], getLatex(result))
  result$matrix <- sub("end\\{pmatrix\\}", wrapper[2], matrix)
  result$wrapper <- wrapper

  if (!simplify) {
    return(result)
  } else {
    return(paste0("\\frac{1}{", det, "} \n",
                  getLatex(result)))
  }
}

if(FALSE) {
library(matlib)
library(testthat)
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
t(M)   # this error doesn't seem expected
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
t(Y)
X %*% Y

# element-wise multiplication
tY <- t(Y)
X * tY

# scalar
a <- symbolicMatrix('a', nrow=1, ncol=1)
a * X
X * a

A <- symbolicMatrix(matrix(letters[1:4], 2, 2, byrow=TRUE))
determinant(A)

B <- symbolicMatrix(matrix(letters[1:9], 3, 3, byrow=TRUE))
determinant(B)

(C <- symbolicMatrix(matrix(1:9, nrow=3, ncol=3)))
(D <- symbolicMatrix(matrix(11:19, nrow=3, ncol=3)))
as.numeric(C)
as.numeric(D)
(E <- C + D)
as.numeric(E)
(F <- C %*% D)
as.numeric(F)

(G <- symbolicMatrix(matrix(letters[1:4], 2, 2)))
expect_error(as.numeric(G), 'matrix cannot be coerced')
as.numeric(G, locals=list(a=1, b=2, c=3, d=4))

A <- symbolicMatrix(nrow=2, ncol=3)
B <- symbolicMatrix(nrow=2, ncol=3)
A + B

C <- symbolicMatrix()
D <- symbolicMatrix()
expect_error(C + D, "does not have numeric dimensions")

A <- symbolicMatrix(matrix(letters[1:9], 3, 3, byrow=TRUE))
A
solve(A)
Eqn(solve(A, simplify=TRUE))

# check again Wolfram Alpha: www.wolframalpha.com
# inverse of {{a, b, c}, {d, e, f}, {g, h, i}}

B <- symbolicMatrix(matrix(letters[1:4], 2, 2, byrow=TRUE),
                    matrix="\\bmatrix")
B
solve(B)
Eqn(solve(B, simplify=TRUE))

# example from <https://www.vedantu.com/maths/inverse-of-a-matrix-using-minors-cofactors-and-adjugate>
X <- symbolicMatrix(matrix(c(3,2,0,1,1,1,2,-2,1), 3, 3))
X
solve(X)
MASS::fractions(as.numeric(solve(X)))
MASS::fractions(solve(as.numeric(X))) # check
MASS::fractions(as.numeric(solve(A),
                           locals=list(a=3, d=2, g=0,
                                       b=1, e=1, h=1,
                                       c=2, f=-2, i=1)))

}
