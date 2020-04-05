# From: https://stackoverflow.com/questions/36798301/r-compute-cross-product-of-vectors-physics?noredirect=1&lq=1


#' Generalized Vector Cross Product
#'
#' Given two linearly independent length 3 vectors **a** and **b**, the cross product, \eqn{\mathbf{a} \times \mathbf{b}}
#' (read "a cross b"), is a vector that is perpendicular to both **a** and **b**
#' thus normal to the plane containing them.
#'
#' A generalization of this idea applies to two or more dimensional vectors.
#'
#' @param ... N-1 linearly independent vectors of the same length, N.
#'
#' @return    Returns the generalized vector cross-product, a vector of length N.
#' @export
#' @author Matthew Lundberg, in a [Stack Overflow post][https://stackoverflow.com/questions/36798301/r-compute-cross-product-of-vectors-physics]
#' @details See: [https://en.wikipedia.org/wiki/Cross_product] for geometric and algebraic properties.
#'  \if{html}{\figure{CrossProduct.png}{Cross-product of 3D vectors}}
#'  \if{latex}{\figure{CrossProduct.png}{options: width=0.5in}}
#' @examples
#' xprod(1:3, 4:6)
#'
#' # This works for an dimension
#' xprod(c(0,1))             # 2d
#' xprod(c(1,0,0), c(0,1,0)) # 3d
#' xprod(c(1,1,1), c(0,1,0)) # 3d
#' xprod(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0)) # 4d
#'
xprod <- function(...) {
  args <- list(...)

  # Check for valid arguments

  if (length(args) == 0) {
    stop("No data supplied")
  }
  len <- unique(sapply(args, FUN=length))
  if (length(len) > 1) {
    stop("All vectors must be the same length")
  }
  if (len != length(args) + 1) {
    stop("Must supply N-1 vectors of length N")
  }

  # Compute generalized cross product by taking the determinant of sub-matricies

  m <- do.call(rbind, args)
  sapply(seq(len),
         FUN=function(i) {
           det(m[,-i,drop=FALSE]) * (-1)^(i+1)
         })
}
