simplify <- function(object, simplify0s=TRUE, simplify1s=TRUE, 
                     sparse=FALSE, ...){
  UseMethod("simplify")
}

simplify.default <- function(object, simplify0s=TRUE, simplify1s=TRUE, 
                             sparse=FALSE, ...){
  if (simplify1s){
    # remove leading 1
    object <- gsub(" *[^0-9]1 *\\\\cdot *", " ", 
                   paste0(" ", object) )
    # removing trailing 1
    object <- gsub(" *\\\\cdot *1 {1,}", " ", 
                   paste0(object, " ")) 
    # remove leading -1
    object <- gsub(" *\\(*-[^0-9]*1\\)* *\\\\cdot *", " -", 
                   paste0(" ", object))
    # removing trailing -1
    trailNeg1 <- grepl(" *\\\\cdot \\(*-1*\\)* {1,}", object)
    object <- gsub(" *\\\\cdot \\(*-1*\\)* {1,}", " ", 
                   object) 
    if (any(trailNeg1)) object <- paste0("-", sapply(object, parenthesize))
  }
  if (simplify0s){
    # remove items with leading 0
    object <- gsub(" *[^1-9_{}}]*0 \\\\cdot (\\(*.*\\))* *[^+-]*", 
                   " ", object)
    # remove items with trailing 0
    object <- gsub(" *(\\(*.*\\))*.*\\\\cdot *[^1-9_{}}]*0", 
                   " ", object) 
    object <- if (sparse){
      object <- trimws(object)
      sub("^0$", "", object) # replace 0 with empty
    } else {
      sub("^ *$", "0", object) # replace empty with 0
    }
  }
  object <- sub("^ *\\+ *", "", object) # remove stray + 
  object <- trimws(object)
  object
}

simplify.latexMatrix <- function(object, simplify0s=TRUE, simplify1s=TRUE, 
                             sparse=FALSE, ...){
  X <- getBody(object)
  dim <- dim(X)
  X <- simplify(X, simplify0s=simplify0s, 
                simplify1s=simplify1s, sparse=sparse)
  X <- matrix(X, dim[1], dim[2])
  X <- latexMatrix(X)
  X <- matlib:::updateWrapper(X, getWrapper(object))
  X$dim <- Dim(object)
  X
}

matmult <- function(X, ...){
  UseMethod("matmult")
}

matmult.latexMatrix <- function(X, ..., simplify1s=TRUE, 
                                simplify0s=TRUE, sparse=FALSE){
  matrices <- list(...)
  for (i in seq_along(matrices)){
    X <- X %*% matrices[[i]]
  }
  if (any(simplify1s, simplify0s, sparse)) {
    simplify(X, simplify1s=simplify1s, simplify0s=simplify0s,
             sparse=sparse)
  } else {
    X
  }
}

if (FALSE){
  
cell <- "0 \\cdot \\beta_{0,y_{1}} + 1 \\cdot \\beta_{1,y_{1}} + 0 \\cdot \\beta_{2,y_{1}} + 0 \\cdot \\beta_{3,y_{1}}"
cell
simplify(cell)
simplify("0 \\cdot (a + b)")
simplify("0 \\cdot \\beta_{1,y_{1}}")
simplify("1 \\cdot (a + b)")
simplify("1 \\cdot \\beta_{1,y_{1}}")
simplify("\\beta_{1,y_{1}} \\cdot 1") 
simplify("\\beta_{1,y_{1}} \\cdot 0")
simplify("(a + b) \\cdot 1")
simplify("(a + b) \\cdot 0")
simplify("0 \\cdot (a + b)", sparse=TRUE)
simplify("(a + b) \\cdot 1", sparse=TRUE)

C <- latexMatrix(matrix(c(0, 1, 0, 0,
                          0, 0, 1, 0), nrow=2, byrow=TRUE), 
                 matrix = "bmatrix")
B <- latexMatrix('\\beta', ncol = 3, nrow=4, 
                 comma=TRUE, prefix.col = 'y_',
                 zero.based=c(TRUE, FALSE))
C %*% B
simplify(C %*% B)

simplify(latexMatrix(diag(3)), sparse=TRUE)

D <- diag(4)
D[3, 3] <- 0
D <- latexMatrix(D)
D %*% B
simplify(D %*% B)

R <- latexMatrix("r", nrow=2, ncol=2)
S <- latexMatrix("s", nrow=2, ncol=2)
T <- latexMatrix("t", nrow=2, ncol=2)

matmult(R)
matmult(R, S)
matmult(R, S, T)

matmult(D, B)
matmult(D, B, simplify1s=FALSE, simplify0s=FALSE)

E <- latexMatrix(matrix(c(-1, 0, 1, 1, 0, -1, 0, 1, 0), 3, 3))
E

matmult(B, E)

B %*% E

F <- latexMatrix(matrix(c(-1, 0, 1, 0, 0, -1, 0, 1), ncol=4))
F

matmult(F, B)

}
