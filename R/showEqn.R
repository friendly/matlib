################################
# -- Show A, b as equations
################################

#' Show Matrices (A, b) as Linear Equations
#'
#' Shows what matrices \eqn{\mathbf{A}, \mathbf{b}} look like as the system of linear equations,
#' \eqn{\mathbf{A x} = \mathbf{b}}, but written out
#' as a set of equations.
#'
#' @param A either the matrix of coefficients of a system of linear equations, or the matrix \code{cbind(A,b)}.
#'   The matrix can be numeric or character.
#'   Alternatively, can be of class \code{'lm'} to print the equations for the design matrix in a linear
#'   regression model
#' @param b if supplied, the vector of constants on the right hand side of the equations. When omitted
#'   the values \code{b1, b2, ..., bn} will be used as placeholders
#' @param vars a numeric or character vector of names of the variables.
#'        If supplied, the length must be equal to the number of unknowns in the equations.
#'        The default is \code{paste0("x", 1:ncol(A)}.
#' @param simplify logical; try to simplify the equations?
#' @param reduce logical; only show the unique linear equations
#' @param fractions logical; express numbers as rational fractions, using the \code{\link[MASS]{fractions}}
#'    function; if you require greater accuracy, you can set the \code{cycles} (default 10)
#'    and/or \code{max.denominator} (default 2000) arguments to \code{fractions} as a global option, e.g.,
#'    \code{options(fractions=list(cycles=100, max.denominator=10^4))}.
#' @param latex logical; print equations in a form suitable for LaTeX output?
#' @return a one-column character matrix, one row for each equation
#' @author Michael Friendly, John Fox, and Phil Chalmers
#' @references Fox, J. and Friendly, M. (2016). "Visualizing Simultaneous Linear Equations, Geometric Vectors, and
#' Least-Squares Regression with the matlib Package for R". \emph{useR Conference}, Stanford, CA, June 27 - June 30, 2016.
#' @seealso \code{\link{plotEqn}}, \code{\link{plotEqn3d}}, \code{\link{symbolicMatrix}}
#' @export
#' @importFrom methods is
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
#'
#'  # lower triangle of equation with zeros omitted (for back solving)
#'   A <- matrix(c(2, 1, 2,
#'                -3, -1, 2,
#'                -2,  1, 2), 3, 3, byrow=TRUE)
#'   U <- LU(A)$U
#'   showEqn(U, simplify=TRUE, fractions=TRUE)
#'   showEqn(U, b, simplify=TRUE, fractions=TRUE)
#'
#'  ####################
#'  # Linear models Design Matricies
#'   data(mtcars)
#'   ancova <- lm(mpg ~ wt + vs, mtcars)
#'   summary(ancova)
#'   showEqn(ancova)
#'   showEqn(ancova, simplify=TRUE)
#'   showEqn(ancova, vars=round(coef(ancova),2))
#'   showEqn(ancova, vars=round(coef(ancova),2), simplify=TRUE)
#'
#'   twoway_int <- lm(mpg ~ vs * am, mtcars)
#'   summary(twoway_int)
#'   car::Anova(twoway_int)
#'   showEqn(twoway_int)
#'   showEqn(twoway_int, reduce=TRUE)
#'   showEqn(twoway_int, reduce=TRUE, simplify=TRUE)
#'
#'   # Piece-wise linear regression
#'   x <- c(1:10, 13:22)
#'   y <- numeric(20)
#'   y[1:10] <- 20:11 + rnorm(10, 0, 1.5)
#'   y[11:20] <- seq(11, 15, len=10) + rnorm(10, 0, 1.5)
#'   plot(x, y, pch = 16)
#'
#'   x2 <- as.numeric(x > 10)
#'   mod <- lm(y ~ x + I((x - 10) * x2))
#'   summary(mod)
#'   lines(x, fitted(mod))
#'   showEqn(mod)
#'   showEqn(mod, vars=round(coef(mod),2))
#'   showEqn(mod, simplify=TRUE)
#'

showEqn <- function(A, b, vars, simplify=FALSE, reduce = FALSE,
                    fractions=FALSE, latex = FALSE) {
  ndigits <- function(x){
    x <- sub("[*].*$", "", x)
    nchar(gsub("[^[:digit:]]", "", x))
  }
  if(is(A, 'lm')){
  	X <- model.matrix(A)
  	return(showEqn(A=X, b=b, vars=vars, simplify=simplify, fractions=fractions,
  				   reduce=reduce, latex=latex))
  }
  if (missing(b)) {
    b <- paste0('b', 1:nrow(A))
  }
  else b <- if (fractions){
    as.character(Fractions(b))
  } else as.character(b)
  if (missing(vars)) vars <- paste0("x", 1:ncol(A))
  V <- substr(vars[1], 1, 1)
  pat2 <- gsub("x", V, "0\\*x\\d")
  res <- character(nrow(A))
  res.matrix <- matrix("", nrow(A), ncol(A))
  if (fractions) A <- as.character(Fractions(A))
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      res.matrix[i, j] <- paste0(A[i, j], "*", vars[j])
      if (j > 1) res.matrix[i, j] <- paste0(" + ", res.matrix[i, j])
      res.matrix[i, j] <- gsub("+ -", "- ", res.matrix[i, j], fixed=TRUE)  # map "+ -3" -> "-3"
      if (simplify) {
          if (ndigits(res.matrix[i, j]) > 1) next
          if(j == 1L){
              res.matrix[i, j] <- gsub("1*", "", res.matrix[i, j], fixed=TRUE) # "1*x" -> "x"
              res.matrix[i, j] <- gsub(pat2, "", res.matrix[i, j])  # "0*x -> ""
          } else {
              res.matrix[i, j] <- gsub("+ 1*", "+ ", res.matrix[i, j], fixed=TRUE)
              res.matrix[i, j] <- if(grepl(" \\+ 0\\*", res.matrix[i, j])) "" else res.matrix[i, j]  # "+ 0*x" -> ""
          }
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
    if(!reduce){
        b[i] <- paste0(paste(rep(" ", max.chars.b - nchar(b[i])), collapse=""), b[i])
        res[i] <- paste(res[i], " = ", b[i])
    }
  }
  if(reduce){
      res <- unique(res)
      b <- paste0('b', 1L:length(res))
      res <- paste(res, " = ", b)
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
  if(latex){
      cat('\\end{array}')
      return(NULL)
  }
  invisible(res)
}


