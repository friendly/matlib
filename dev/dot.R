#' Dot Product of Latex Vectors
#' 
#' @description
#' 
#' The result of matrix multiplication, \eqn{\mathbf{C} = \mathbf{A} \: \mathbf{B}}
#' is composed of the vector dot products of each \emph{row} of \eqn{\mathbf{A}} with
#' each \emph{column} of \eqn{\mathbf{B}},
#' \deqn{c_{ij} = \mathbf{a}_i^top \mathbf{b}_j 
#'              = \Sigma}
#' 
#' This function computes this product symbolically in LaTeX notation for
#' numeric and character vectors, simplifying the result if \code{simplify = TRUE.}

#' The LaTeX symbol for multiplication (\code{"\\cdot"} by default)
#' can be changed by changing \code{options(latexMultSymbol)},
#' e.g, \code{options(latexMultSymbol = "\\times")}.
#' 
#' @param x A numeric or character vector
#' @param y A numeric or character vector. The lengths of \code{x} and \code{y} must
#'          be equal
#' @param simplify logical; if \code{TRUE}, terms involving the numeric constants 
#'          \code{-1, 0, 1} are simplified
#' @returns A character string containing the LaTeX expression
#' @export


dot <- function(x, y, simplify = TRUE) {
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
  res
}


if (FALSE){
  num <- -1:2
  chr <- letters[1:4]
  
  dot(num, num)  # OK
  dot(num, chr)  # OK
  dot(chr, num)  # OK
  dot(chr, chr)  # OK
  
  dot(num, num, simplify = FALSE) 
  dot(num, chr, simplify = FALSE)
  dot(chr, num, simplify = FALSE)
  dot(chr, chr, simplify = FALSE)

  # change the mult symbol  
  opt <- options(latexMultSymbol = "\\times") 
  options("latexMultSymbol")
  dot(num, num)
  dot(num, chr)
  dot(chr, chr)
  options(opt)
}
