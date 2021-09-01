#############################################################################
# -- Solve and display solutions for systems of linear simultaneous equations
#############################################################################

#' Solve and Display Solutions for Systems of Linear Simultaneous Equations
#'
#' Solve the equation system \eqn{Ax = b}, given the coefficient matrix
#' \eqn{A} and right-hand side vector \eqn{b}, using \code{link{gaussianElimination}}.
#' Display the solutions using \code{\link{showEqn}}.
#'
#' @details This function mimics the base function \code{\link[base]{solve}} when supplied with two arguments,
#'    \code{(A, b)}, but gives a prettier result, as a set of equations for the solution.  The call
#'    \code{solve(A)} with a single argument overloads this, returning the inverse of the matrix \code{A}.
#'    For that sense, use the function \code{\link{inv}} instead.
#'
#' @param A, the matrix of coefficients of a system of linear equations
#' @param b, the vector of constants on the right hand side of the equations. The default is a vector of zeros,
#'        giving the homogeneous equations \eqn{Ax = 0}.
#' @param vars a numeric or character vector of names of the variables.
#'        If supplied, the length must be equal to the number of unknowns in the equations.
#'        The default is \code{paste0("x", 1:ncol(A)}.
#' @param verbose, logical; show the steps of the Gaussian elimination algorithm?
#' @param simplify logical; try to simplify the equations?
#' @param fractions logical; express numbers as rational fractions, using the \code{\link[MASS]{fractions}} 
#'    function; if you require greater accuracy, you can set the \code{cycles} (default 10)
#'    and/or \code{max.denominator} (default 2000) arguments to \code{fractions} as a global option, e.g.,
#'    \code{options(fractions=list(cycles=100, max.denominator=10^4))}.
#' @param ..., arguments to be passed to \code{link{gaussianElimination}} and \code{\link{showEqn}}
#' @return the function is used primarily for its side effect of printing the solution in a
#'  readable form, but it invisibly returns the solution as a character vector
#' @export
#' @author John Fox
#' @seealso \code{\link{gaussianElimination}}, \code{\link{showEqn}} \code{\link{inv}}, \code{\link[base]{solve}}
#' @examples
#'   A1 <- matrix(c(2, 1, -1,
#'                -3, -1, 2,
#'                -2,  1, 2), 3, 3, byrow=TRUE)
#'   b1 <- c(8, -11, -3)
#'   Solve(A1, b1) # unique solution
#'
#'   A2 <- matrix(1:9, 3, 3)
#'   b2 <- 1:3
#'   Solve(A2,  b2, fractions=TRUE) # underdetermined
#'
#'   b3 <- c(1, 2, 4)
#'   Solve(A2, b3, fractions=TRUE) # overdetermined

Solve <- function(A, b = rep(0, nrow(A)), vars, verbose=FALSE, simplify=TRUE, fractions=FALSE, ...){
  solution <- gaussianElimination(A, b, verbose=verbose, fractions=fractions, ...)
  showEqn(solution[, - ncol(solution)], solution[, ncol(solution)], vars=vars, simplify=simplify,
          fractions=fractions, ...)
}
