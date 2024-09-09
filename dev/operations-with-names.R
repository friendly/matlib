numericDimensions <- matlib:::numericDimensions
parenthesize <- matlib:::parenthesize
updateWrapper <- matlib:::updateWrapper
getLatexMultSymbol <- matlib:::getLatexMultSymbol
isOdd <- matlib:::isOdd 

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
  
  numericDimensions(A)
  
  dimnames <- dimnames(A)
  for (M in matrices)   {
    numericDimensions(M)
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
      A <- matrix(paste(sapply(A, parenthesize), "+", 
                        sapply(M, parenthesize)), 
                  dimA[1L], dimA[2L])
    }
  }
  A <- latexMatrix(A)
  A <- updateWrapper(A, wrapper)
  Dimnames(A) <- dimnames
  A
}


matdiff.latexMatrix <- function(A, B=NULL, as.numeric=TRUE, ...){
  
  wrapper <- getWrapper(A)
  
  # unary -
  if (is.null(B)){
    numericDimensions(A)
    dimnames <- dimnames(A)
    if (as.numeric && is.numeric(A)){
      A <- as.numeric(A)
      A <- -A
    } else {
      A <- getBody(A)
      dimA <- dim(A)
      A <- matrix(paste("-", sapply(A, parenthesize)), dimA[1L], dimA[2L])
    }
    A <- latexMatrix(A)
    A <- updateWrapper(A, getWrapper(A))
    Dimnames(A) <- dimnames
    return(A)
  }
  if (!inherits(B, "latexMatrix")){
    stop(deparse(substitute(B)),
         " is not of class 'latexMatrix'")
  }
  numericDimensions(A)
  numericDimensions(B)
  if (!isTRUE(all.equal(dimnames(A), dimnames(B)))){
    stop("matrix dimension names don't match")
  }
  dimnames <- dimnames(A)
  dimA <- Dim(A)
  dimB <- Dim(B)
  if (!all(dimA == dimB)) 
    stop('matricies are not conformable for subtraction')
  if (as.numeric && is.numeric(A) && is.numeric(B)){
    A <- as.numeric(A)
    B <- as.numeric(B)
    A <- A - B
  } else {
    A <- getBody(A)
    B <- getBody(B)
    A <- matrix(paste(sapply(A, parenthesize), "-", 
                      sapply(B, parenthesize)), 
                dimA[1L], dimA[2L])
  }
  A <- latexMatrix(A)
  A <- updateWrapper(A, wrapper)
  Dimnames(A) <- dimnames
  A
}


`-.latexMatrix` <- function(e1, e2){
  if (missing(e2)) e2 <- NULL
  matdiff(e1, e2)
}


`*.latexMatrix` <- function (e1, e2) {
  if (inherits(e1, "latexMatrix") && inherits(e2, "latexMatrix")) 
    stop("both arguments of * cannot be 'latexMatrix' objects")
  swapped <- if (inherits(e1, "latexMatrix")) {
    swap <- e1
    e1 <- e2
    e2 <- swap
    TRUE
  } else {
    FALSE
  }
  if (!is.vector(e1) || length(e1) != 1) 
    stop("one argument to * must be a scalar")
  numericDimensions(e2)
  
  latexMultSymbol <- getLatexMultSymbol()
  
  A <- getBody(e2)
  dimA <- dim(A)
  wrapper <- getWrapper(e2)
  dimnames <- dimnames(e2)
  result <- matrix(if (swapped) {
    paste(sapply(A, parenthesize), latexMultSymbol, e1)
  } else{
    paste(e1, latexMultSymbol, sapply(A, parenthesize))
  },
  dimA[1L], dimA[2L])
  result <- latexMatrix(result)
  result <- updateWrapper(result, getWrapper(e2))
  result$dim <- Dim(e2)
  Dimnames(result) <- dimnames
  result
}


matmult <- function(X, ...){
  UseMethod("matmult")
}

matmult.latexMatrix <- function(X, ..., simplify=TRUE, 
                                as.numeric=TRUE){
  
  matrices <- list(...)
  
  if (any(sapply(matrices, function(x) !inherits(x, "latexMatrix")))){
    stop("arguments are not all of class 'latexMatrix'")
  }
  
  numericDimensions(X)
  for (M in matrices)   numericDimensions(M)
  
  n.matrices <- length(matrices)
  if (n.matrices > 1){
    for (i in 1:(n.matrices - 1)){
      if (!isTRUE(all.equal(colnames(M[[i]]), 
                            rownames(M[[i + 1]])))){
        stop("matrix dimension names don't match")
      }
    }
  }
  dimnames <- list(rownames = rownames(X),
                   colnames = colnames(matrices[[n.matrices]]))
  wrapper <- getWrapper(X)
  
  if (as.numeric && is.numeric(X) && all(sapply(matrices, is.numeric))){
    X <- as.numeric(X)
    matrices <- lapply(matrices, as.numeric)
    for (i in seq_along(matrices)){
      X <- X %*% matrices[[i]]
    }
  } else {
    
    X <- getBody(X)
    
    for (M in matrices){
      
      Y <- getBody(M)
      if (ncol(X) != nrow(Y)){
        stop('matricies are not conformable for multiplication')
      }
      
      Z <- matrix("", nrow(X), ncol(Y))
      
      for (i in 1:nrow(X)){
        for (j in 1:ncol(Y)){
          for (k in 1:ncol(X)){
            Z[i, j] <- Dot(X[i, ], Y[, j], simplify=simplify)
          }
        }
      }
      X <- Z
    }
  }
  X <- latexMatrix(X)
  X <- updateWrapper(X, wrapper)
  Dimnames(X) <- dimnames
  return(X)
  
}

`%*%.latexMatrix` <- function(x, y){
  matmult(x, y)
}


t.latexMatrix <- function(x){
  numericDimensions(x)
  result <- latexMatrix(t(getBody(x)))
  result <- updateWrapper(result, getWrapper(x))
  dimnames <- dimnames(x)
  Dimnames(result) <- list(rownames = dimnames[[2]],
                           colnames = dimnames[[1]])
  result
}

`[.latexMatrix` <- function(x, i, j, ..., drop){
  numericDimensions(x)
  X <- getBody(x)
  if (!is.null(nms <- rownames(x))) rownames(X) <- nms
  if (!is.null(nms <- colnames(x))) colnames(X) <- nms
  X <- X[i, j, drop=FALSE]
  X <- latexMatrix(X)
  updateWrapper(X, getWrapper(x))
}

matpower <- function(X, power, ...){
  UseMethod("matpower")
}

matpower.latexMatrix <- function(X, power, simplify=TRUE, 
                                 as.numeric=TRUE, ...){
  
  numericDimensions(X)
  dimX <- Dim(X)
  dimnames <- dimnames(X)
  
  if (dimX[1] != dimX[2]) stop ("X is not square")
  if (power != round(power) || power < -1) 
    stop("'power' must be an integer >= -1")
  
  wrapper <- getWrapper(X)
  
  if (power == 0){
    result <- latexMatrix(diag(dimX[1]))
    result <- updateWrapper(result, wrapper)
    Dimnames(result) <- dimnames
    return(result)
  }
  
  if (as.numeric && is.numeric(X)){
    X <- as.numeric(X)
    Xp <- if (power == -1){
      solve(X)
    } else {
      result <- diag(dimX[1])
      for (i in 1:power){
        result <- result %*% X
      }
      result
    }
    Xp <- latexMatrix(Xp)
  } else {
    Xp <- if (power == -1) {
      solve(X, simplify=simplify, as.numeric=as.numeric)
    } else {
      result <- latexMatrix(diag(dimX[1]))
      for (i in 1:power){
        result <- matmult(result, X, simplify=simplify, 
                          as.numeric=as.numeric)
      }
      result
    }
  }
  if (inherits(Xp, "latexMatrix")){
    Xp <- updateWrapper(Xp, wrapper)
    Dimnames(Xp) <- dimnames
  }
  return(Xp)
}


solve.latexMatrix <- function (a, b, simplify=FALSE, as.numeric=TRUE,
                               frac=c("\\dfrac", "\\frac", "\\tfrac", "\\cfrac"),
                               ...) {
  
  # symbolic matrix inverse by adjoint matrix and determinant
  
  frac <- match.arg(frac)
  
  numericDimensions(a)
  if (Nrow(a) != Ncol(a)) stop("matrix 'a' must be square")
  if (!missing(b)) warning("'b' argument to solve() ignored")
  
  dimnames <- dimnames(a)
  
  if (as.numeric && is.numeric(a)){
    a.inv <- solve(as.numeric(a))
    a.inv <- latexMatrix(a.inv)
    a.inv <- updateWrapper(a.inv, getWrapper(a))
    Dimnames(a.inv) <- dimnames
    return(a.inv)
  }
  
  det <- determinant(a)
  A <- getBody(a)
  n <- nrow(A)
  indices <- 1:n
  A_inv <- matrix("", n, n)
  
  for (i in 1:n){
    for (j in 1:n){
      A_ij <- latexMatrix(A[indices[-i], indices[-j], drop=FALSE])
      A_inv[i, j] <- if (Nrow(A_ij) == 1) { # cofactors
        A[indices[-i], indices[-j]]
      } else{
        determinant(A_ij)
      }
      if (isOdd(i + j)) A_inv[i, j] <- paste0("-", parenthesize(A_inv[i, j]))
      if (!simplify) A_inv[i, j] <- paste0(frac, "{", A_inv[i, j],
                                           "}{", det, "}")
    }
  }
  
  A_inv <- t(A_inv) # adjoint
  result <- latexMatrix(A_inv)
  result <- updateWrapper(result, getWrapper(a))
  Dimnames(result) <- dimnames
  
  if (!simplify) {
    return(result)
  } else {
    return(paste0("\\frac{1}{", det, "} \n",
                  getLatex(result)))
  }
}


inverse <- function(X, ...){
  UseMethod("inverse")
}

inverse.latexMatrix <- function(X, ..., as.numeric=TRUE, 
                                simplify=TRUE){
  matpower(X, -1, as.numeric=as.numeric, simplify=simplify)
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
  
  A - B
  -A
  A - C
  
  2*A
  
  X <- latexMatrix(nrow=3, ncol=2, rownames=letters[1:3], colnames=LETTERS[1:2])
  t(X)
  X["a", "B"]
  X[c("a", "c"), "B"]
  
  XX <- latexMatrix(nrow=3, ncol=2, rownames=c("a", "b", "a"), colnames=LETTERS[1:2])
  XX["a", "B"]
  
  Y <- latexMatrix("y", nrow=2, ncol=4, rownames=letters[4:5], colnames=LETTERS[3:6])
  X
  Y
  X %*% Y
  
  matpower(A, 2)
  matpower(A, -1)
  
  inverse(A)
  inverse(A, simplify=FALSE)
}
