#' Various Functions and Operators for \code{"latexMatrix"} Objects
#'
#' @description
#' Arithmetic and other functions and operators provided to manipulate 
#' \code{"latexMatrix"} objects, both symbolic and numeric:
#' \itemize{
#' \item \code{matsum()} and \code{+}, matrix addition;
#' \item \code{matdiff()} and \code{-}, matrix subtraction and negation;
#' \item \code{*}, product of a scalar and a matrix;
#' \item \code{Dot()}, inner product of two vectors;
#' \item \code{matprod()} and \code{\%*\%}, matrix product;
#' \item \code{matpower()} and \code{^}, powers (including inverse) of 
#' a square matrix;
#' \item \code{solve()} and \code{inverse()}, matrix inverse of a square matrix;
#' \item \code{t()}, transpose;
#' \item \code{determinant()} of a square matrix;
#' \item \code{kronecker()} and \code{\%O\%}, the Kronecker product.
#' }
#' @name latexMatrixOperations
#' 
#' @details
#' These operators and functions only apply to \code{"latexMatrix"} objects
#' of definite (i.e., numeric) dimensions. When there are both a funcion and an
#' operator (e.g., \code{matmult()} and \code{\%*\%}), the former is more
#' flexible via optional arguments and the latter calls the former with default 
#' arguments.
#' 
#' The result of matrix multiplication, \eqn{\mathbf{C} = \mathbf{A} \: \mathbf{B}}
#' is composed of the vector inner (dot) products of each \emph{row} of \eqn{\mathbf{A}} with
#' each \emph{column} of \eqn{\mathbf{B}},
#' \deqn{c_{ij} = \mathbf{a}_i^\top \mathbf{b}_j 
#'              = \Sigma_k a_{ik} \cdot b_{kj}}
#' 
#' The \code{Dot()} function computes the inner product symbolically in LaTeX notation for
#' numeric and character vectors, simplifying the result if \code{simplify = TRUE.}
#' The LaTeX symbol for multiplication (\code{"\\cdot"} by default)
#' can be changed by changing \code{options(latexMultSymbol)},
#' e.g, \code{options(latexMultSymbol = "\\\\times")} (note the double-backslash).
#' 

#' @author John Fox
#' @seealso \code{\link{latexMatrix}}

# # for debugging:
# numericDimensions <- matlib:::numericDimensions
# parenthesize <- matlib:::parenthesize
# updateWrapper <- matlib:::updateWrapper
# getLatexMultSymbol <- matlib:::getLatexMultSymbol

#' @param e1 a \code{"latexMatrix"} object; or for \code{*} a scalar;
#' @param e2 a \code{"latexMatrix"} object;  for \code{*} a scalar;
#' for \code{^} an integer power \code{>= -1} to raise a square matrix 
#' @param A a \code{"latexMatrix"} object
#' @param B a \code{"latexMatrix"} object
#' @param X a \code{"latexMatrix"} object
#' @param Y a \code{"latexMatrix"} object
#' @param x for \code{Dot} a numeric or character vector; 
#' otherwise a \code{"latexMatrix"} object
#' @param y for \code{Dot} a numeric or character vector; 
#' otherwise a \code{"latexMatrix"} object
#' @param simplify if \code{TRUE} (the default), an attempt is made
#' to simplify the result slightly; for \code{solve()}, 
#' return a LaTeX expression with the inverse of the determinant in
#' front of the adjoint matrix rather than a \code{"latexMatrix"} object in which each
#' element of the adjoint matrix is divided by the determinant
#' @param as.numeric if \code{TRUE} (the default) and the matrices to be multiplied, added, etc., can be
#' coerced to numeric, matrix multiplication, addition, etc., is performed numerically;
#' supercedes \code{simplify}
#' @param power to raise a square matrix, an integer \code{>= -1}. 
#' @param ... for \code{matmult()} and \code{sum()} zero or more 
#' \code{"latexMatrix"} objects; otherwise arguments to be passed down
#' @param a a \code{"latexMatrix"} object representing a square matrix
#' @param b ignored; to match the \code{\link{solve}()} generic
#' @param frac LaTeX command to use in forming fractions; the default
#' is \code{"\\dfrac"}
#' @param logarithm to match the generic \code{\link{determinant}()} function,
#' ignored
#' @param FUN to match the \code{\link{kronecker}()} generic, ignored
#' @param make.dimnames to match the \code{\link{kronecker}()} generic, ignored
#'   
#' @examples
#' A <- latexMatrix(symbol="a", nrow=2, ncol=2)
#' B <- latexMatrix(symbol="b", nrow=2, ncol=2)
#' A
#' B
#' A + B
#' A - B
#' "a" * A
#' C <- latexMatrix(symbol="c", nrow=2, ncol=3)
#' A %*% C
#' t(C)
#' determinant(A)
#' cat(solve(A, simplify=TRUE))
#' D <- latexMatrix(matrix(letters[1:4], 2, 2))
#' D
#' as.numeric(D, locals=list(a=1, b=2, c=3, d=4))
#' X <- latexMatrix(matrix(c(3, 2, 0, 1, 1, 1, 2,-2, 1), 3, 3))
#' X
#' as.numeric(X)
#' MASS::fractions(as.numeric(inverse(X)))
#' (d <- determinant(X))
#' eval(parse(text=(gsub("\\\\cdot", "*", d))))
#' X <- latexMatrix(matrix(1:6, 2, 3), matrix="bmatrix")
#' I3 <- latexMatrix(diag(3))
#' I3 %X% X
#' kronecker(I3, X, sparse=TRUE)
#' 
#' (E <- latexMatrix(diag(1:3)))
#' # equivalent:
#' X %*% E
#' matmult(X, E)
#' 
#' matmult(X, E, simplify=FALSE, as.numeric=FALSE)
#' 
#' # equivalent:
#' X %*% E %*% E
#' matmult(X, E, E)
#' 
#' # equivalent:
#' E^-1
#' inverse(E)
#' solve(E)
#' 
#' solve(E, as.numeric=FALSE) # details
#' 
#' # equivalent
#' E^3
#' matpower(E, 3)
#' 
#' matpower(E, 3, as.numeric=FALSE)

#' @returns All of these functions return \code{"latexMatrix"} objects, 
#' except for \code{Dot()}, which returns a LaTeX expression as a character string.

#' @rdname latexMatrixOperations
#' @export
matsum <- function(A, ...){
  UseMethod("matsum")
}

#' @rdname latexMatrixOperations
#' @export
matsum.latexMatrix <- function(A, ..., as.numeric=TRUE){
  
  matrices <- list(...)
  
  if (any(sapply(matrices, function(x) !inherits(x, "latexMatrix")))){
    stop("arguments are not all of class 'latexMatrix'")
  }
  
  numericDimensions(A)
  for (M in matrices)   numericDimensions(M)
  
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
  A
}

#' @rdname latexMatrixOperations
#' @export
`+.latexMatrix` <- function(e1, e2){
  matsum(e1, e2)
}

#' @rdname latexMatrixOperations
#' @export
matdiff <- function(A, B, ...){
  UseMethod("matdiff")
}

#' @rdname latexMatrixOperations
#' @export
matdiff.latexMatrix <- function(A, B=NULL, as.numeric=TRUE, ...){
  
  wrapper <- getWrapper(A)
  
  # unary -
  if (is.null(B)){
    numericDimensions(A)
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
    return(A)
  }
  if (!inherits(B, "latexMatrix")){
    stop(deparse(substitute(B)),
         " is not of class 'latexMatrix'")
  }
  numericDimensions(A)
  numericDimensions(B)
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
  A
}

#' @rdname latexMatrixOperations
#' @export
`-.latexMatrix` <- function(e1, e2){
  if (missing(e2)) e2 <- NULL
  matdiff(e1, e2)
}

#' @rdname latexMatrixOperations
#' @export
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
  result <- matrix(if (swapped) {
    paste(sapply(A, parenthesize), latexMultSymbol, e1)
  } else{
    paste(e1, latexMultSymbol, sapply(A, parenthesize))
  },
  dimA[1L], dimA[2L])
  result <- latexMatrix(result)
  result <- updateWrapper(result, getWrapper(e2))
  result$dim <- Dim(e2)
  result
}

#' @rdname latexMatrixOperations
#' @export
Dot <- function(x, y, simplify = TRUE) {
  if (length(x) != length(y)) stop("Vectors must have the same length")
  x <- trimws(x)
  y <- trimws(y)
  latexMultSymbol <- getLatexMultSymbol()
  res <- ""
  for (i in 1:length(x)) {
    if (!simplify) {
      res <- paste0(res,
                    if (i > 1) " + ",
                    parenthesize(x[i]),
                    paste0(" ", latexMultSymbol, " "),
                    parenthesize(y[i]))      
    }
    else {
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
      times <- if(xi == "" || xi == "-" || yi == "") "" else paste0(" ", latexMultSymbol, " ")
      res <- paste0(res,
                    if (nchar(res) > 0) " + ",
                    if (y[i] != "-1" && xi != "-") parenthesize(xi) else xi,
                    times,
                    parenthesize(yi))
    }
  }
  if (res == "") res <- "0"
  if (res == "-") res <- "-1"
  res <- gsub("\\+ *-", "- ", res)
  res
}

#' @rdname latexMatrixOperations
#' @export
matmult <- function(X, ...){
  UseMethod("matmult")
}

#' @rdname latexMatrixOperations
#' @export
matmult.latexMatrix <- function(X, ..., simplify=TRUE, 
                                as.numeric=TRUE){
  
  matrices <- list(...)
  
  if (any(sapply(matrices, function(x) !inherits(x, "latexMatrix")))){
    stop("arguments are not all of class 'latexMatrix'")
  }
  
  numericDimensions(X)
  for (M in matrices)   numericDimensions(M)
  
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
  return(X)
  
}

#' @rdname latexMatrixOperations
#' @export
`%*%.latexMatrix` <- function(x, y){
  matmult(x, y)
}

#' @rdname latexMatrixOperations
#' @export
matpower <- function(X, power, ...){
  UseMethod("matpower")
}

#' @rdname latexMatrixOperations
#' @export
matpower.latexMatrix <- function(X, power, simplify=TRUE, 
                                 as.numeric=TRUE, ...){
  
  numericDimensions(X)
  dimX <- Dim(X)
  if (dimX[1] != dimX[2]) stop ("X is not square")
  if (power != round(power) || power < -1) 
    stop("'power' must be an integer >= -1")
  
  wrapper <- getWrapper(X)
  
  if (power == 0){
    result <- latexMatrix(diag(dimX[1]))
    result <- updateWrapper(result, wrapper)
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
  Xp <- updateWrapper(Xp, wrapper)
  return(Xp)
}

#' @rdname latexMatrixOperations
#' @export
`^.latexMatrix` <- function(e1, e2){
  matpower(e1, e2)
}

#' @rdname latexMatrixOperations
#' @export
inverse <- function(X, ...){
  UseMethod("inverse")
}

#' @rdname latexMatrixOperations
#' @export
inverse.latexMatrix <- function(X, ..., as.numeric=TRUE, 
                                simplify=TRUE){
  matpower(X, -1, as.numeric=as.numeric, simplify=simplify)
}

#' @rdname latexMatrixOperations
#' @export
t.latexMatrix <- function(x){
  numericDimensions(x)
  result <- latexMatrix(t(getBody(x)))
  result <- updateWrapper(result, getWrapper(x))
  result$dim <- rev(Dim(x))
  result
}

#' @rdname latexMatrixOperations
#' @export
determinant.latexMatrix <- function(x, logarithm, ...){
  
  # determinant by cofactors
  
  latexMultSymbol <- getLatexMultSymbol()
  
  # helper function for recursion:
  DET <- function(X){
    if (nrow(X) == 1) {
      as.vector(X)
    } else if (nrow(X) == 2){
      paste0(parenthesize(X[1, 1]), paste0(" ", latexMultSymbol, " "), parenthesize(X[2, 2]), " - ",
             parenthesize(X[1, 2]), paste0(" ", latexMultSymbol, " "), parenthesize(X[2, 1]))
    } else {
      indices <- 1:ncol(X)
      res <- ""
      for (j in indices){
        res <- paste0(res, if (isOdd(j)) " + " else " - ",
                      X[1, j], paste0(" ", latexMultSymbol, " "),
                      parenthesize(DET(X[-1, indices != j]))
        )
      }
      res
    }
  }
  
  numericDimensions(x)
  
  sub("^[ +]*", "", DET(getBody(x)))
}


#' @rdname latexMatrixOperations
#' @export
solve.latexMatrix <- function (a, b, simplify=FALSE, as.numeric=TRUE,
                               frac=c("\\dfrac", "\\frac", "\\tfrac", "\\cfrac"),
                               ...) {
  
  # symbolic matrix inverse by adjoint matrix and determinant
  
  frac <- match.arg(frac)
  
  numericDimensions(a)
  if (Nrow(a) != Ncol(a)) stop("matrix 'a' must be square")
  if (!missing(b)) warning("'b' argument to solve() ignored")
  
  if (as.numeric && is.numeric(a)){
    a.inv <- solve(as.numeric(a))
    a.inv <- latexMatrix(a.inv)
    return(updateWrapper(a.inv, getWrapper(a)))
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
  
  if (!simplify) {
    return(result)
  } else {
    return(paste0("\\frac{1}{", det, "} \n",
                  getLatex(result)))
  }
}

setOldClass("latexMatrix")

#' @rdname latexMatrixOperations
#' @export
setMethod("kronecker", 
          signature(X = "latexMatrix", 
                    Y = "latexMatrix"), 
          function(X, Y, FUN, make.dimnames, ...) {
            
            numericDimensions(X)
            numericDimensions(Y)
            
            latexMultSymbol <- getLatexMultSymbol()
            
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
                              res <- paste0(x, paste0(" ", latexMultSymbol, " "), y)
                              res[zeros] <- "0"
                              res
                            }
            )
            
            result <- latexMatrix(Z, ...)
            result <- updateWrapper(result, getWrapper(X))
            result
          }
)

#' @rdname latexMatrixOperations
#' @export
`%X%` <- function(x, y) methods::kronecker(x, y)
