################################
# -- Show A, b as equations
################################

#' Show Matrices (A, b) as Linear Equations
#'
#' Shows what matrices \eqn{A, b} look like as the system of linear equations, \eqn{A x = b}, but written out
#' as a set of equations.
#'
#' @param A either the matrix of coefficients of a system of linear equations, or the matrix \code{cbind(A,b)}
#' @param b if supplied, the vector of constants on the right hand side of the equations. When ommited 
#'   the values \code{b1, b2, ..., bn} will be used as placeholders
#' @param vars a numeric or character vector of names of the variables.
#'        If supplied, the length must be equal to the number of unknowns in the equations.
#'        The default is \code{paste0("x", 1:ncol(A)}.
#' @param simplify logical; try to simplify the equations?
#' @param fractions logical; express numbers as rational fractions?
#' @param latex logical; print equations in a form suitable for LaTeX output?
#' @return a one-column character matrix, one row for each equation
#' @author Michael Friendly and John Fox
#' @references Fox, J. and Friendly, M. (2016). "Visualizing Simultaneous Linear Equations, Geometric Vectors, and
#' Least-Squares Regression with the matlib Package for R". \emph{useR Conference}, Stanford, CA, June 27 - June 30, 2016.
#' @seealso \code{\link{plotEqn}}, \code{\link{plotEqn3d}}
#' @examples
#'   A <- matrix(c(2, 1, -1,
#'                -3, -1, 2,
#'                -2,  1, 2), 3, 3, byrow=TRUE)
#'   b <- c(8, -11, -3)
#'   showEqn(A, b)
#'   # show numerically
#'   x <- solve(A, b)
#'   showEqn(A, b, vars=x)
#'
#'   showEqn(A, b, simplify=TRUE)
#'   showEqn(A, b, latex=TRUE)
#'   
#'   # lower triangle of equation with zeros ommited (for back solving)
#'   A <- matrix(c(2, 1, 2,
#'                -3, -1, 2,
#'                -2,  1, 2), 3, 3, byrow=TRUE)
#'   U <- LU(A)$U
#'   showEqn(U, simplify=TRUE, fractions=TRUE)
#'   showEqn(U, b, simplify=TRUE, fractions=TRUE)

showEqn <- function(A, b, vars, simplify=FALSE, fractions=FALSE, latex = FALSE) {
  if (missing(b)) {
    b <- paste0('b', 1:nrow(A))
  }
  else b <- if (fractions){
    as.character(MASS::fractions(b))
  } else as.character(b)
  if (missing(vars)) vars <- paste0("x", 1:ncol(A))
  V <- substr(vars[1], 1, 1)
  pat <- gsub("x", V, "0\\*x\\d\\s+[+-]|[+-]\\s+0\\*x\\d")
  pat2 <- gsub("x", V, "0\\*x\\d")
  res <- character(nrow(A))
  res.matrix <- matrix("", nrow(A), ncol(A))
  if (fractions) A <- as.character(MASS::fractions(A))
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      res.matrix[i, j] <- paste0(A[i, j], "*", vars[j])
      if (j > 1) res.matrix[i, j] <- paste0(" + ", res.matrix[i, j])
      res.matrix[i, j] <- gsub("+ -", "- ", res.matrix[i, j], fixed=TRUE)  # map "+ -3" -> "-3"
      if (simplify) {
        res.matrix[i, j] <- gsub("1*", "", res.matrix[i, j], fixed=TRUE)    # "1*x" -> "x"
        res.matrix[i, j] <- gsub(pat, "", res.matrix[i, j])   # "+ 0*x" -> ""
        res.matrix[i, j] <- gsub(pat2, "", res.matrix[i, j])  # "0*x -> ""
        res.matrix[i, j] <- gsub("  ", " ", res.matrix[i, j], fixed=TRUE)
      }
    }
  }
  res.matrix[res.matrix == " "] <- ""
  if (simplify){
    for (i in 1:nrow(res.matrix)){
      for (j in 1:ncol(res.matrix)){
        if (res.matrix[i, j] == "") next
        else{
          res.matrix[i, j] <- sub("^ [+] ", "", res.matrix[i, j])
          break
        }
      }
      if (all(res.matrix[i, ] == "")) res.matrix[i, ncol(res.matrix)] <- "0"
    }
  }
  max.chars <- apply(res.matrix, 2, function(x) max(nchar(x)))
  max.chars.b <- max(nchar(b))
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      res.matrix[i, j] <- paste0(paste(rep(" ", max.chars[j] - nchar(res.matrix[i, j])), collapse=""),
                                 res.matrix[i, j])
    }
    res[i] <- paste0(res.matrix[i, ], collapse="")
    b[i] <- paste0(paste(rep(" ", max.chars.b - nchar(b[i])), collapse=""), b[i])
    res[i] <- paste(res[i], " = ", b[i])
  }
  if(latex){
    res <- gsub('x', 'x_', res)
    res <- gsub('\\*', ' \\\\cdot ', res)
    res <- gsub(' \\+ ', ' &+& ', res)
    res <- gsub(' \\- ', ' &-& ', res)
    res <- gsub(' \\= ', ' &=& ', res)
    res <- paste0(res, ' \\\\')
    cat(sprintf('\\begin{array}{%s}\n', paste0(rep('l', ncol(A)*2+1), collapse = '')))
  }
  for (i in 1:length(res)){
      cat(res[i], "\n")
  }
  if(latex) cat('\\end{array}')
  invisible(res)
}


