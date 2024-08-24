# dot product of latex vectors, simplifying 0s & 1s
# could be used in loop in %*%.latexMatrix to simplify w/o parsing

#' Dot Product of Latex Vectors, Simplifying 0s & 1s
#' 
#' The result of matrix multiplication, \eqn{\mathbf{C} = \mathbf{A} \: \mathbf{B}}
#' is composed of the vector dot products of each row of \eqn{\mathbf{A}} with
#' each column of \eqn{\mathbf{B}},
#' \deqn{c_{ij} = \mathbf{a}_i^top \mathbf{b}_j}
#' This function computes this symbolically in LaTeX notation for
#' \code{"latexMatrix} objects
#' 
#' @param x A numeric or character vector
#' @param y A numeric or character vector. The lengths of \code{x} and \code{y} must
#'          be equal
#' @export
#' @examples
#' num <- 0:2
#' chr <- letters[1:3]
#' 
#' dot(num, num)
#' dot(num, chr)
#' dot(chr, num)
#' dot(chr, chr)

dot <- function(x, y) {
  if (length(x) != length(y)) stop("Vectors must have the same length")
  
  res <- ""
  for (i in 1:length(x)) {
    # ignore terms multiplied by zero
    if (x[i] == "0" | x[i] == 0) next
    if (y[i] == "0" | y[i] == 0) next
    xi <- if(x[i] == "1" | x[i] == 1) "" else x[i]
    yi <- if(y[i] == "1" | y[i] == 1) "" else y[i]
    times <- if(xi == "" | yi == "") "" else " \\cdot "
    res <- paste0(res,
                  if (nchar(res) > 0) " + ",
                  parenthesize(xi),
                  times,
                  parenthesize(yi))
  }
  if (res == "") res <- "0"
  res
}

if (FALSE){
 num <- 0:2
 chr <- letters[1:3]
 
 dot(num, num)
 dot(num, chr)
 dot(chr, num)
 dot(chr, chr)
}