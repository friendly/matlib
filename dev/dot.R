# dot product of latex vectors, simplifying 0s & 1s
# could be used in loop in %*%.latexMatrix to simplify w/o parsing

#' Dot Product of Latex Vectors, Simplifying 0s & 1s
#' 
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