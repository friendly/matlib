# show dual relation between points & lines

library(matlib)

source(here::here("dev", "plotEqn.R"))

A <- matrix(c( 1, 2, 0,
               -1, 2, 1), 3, 2) |>
  print()


b <- c(2, 1, 1)

showEqn(A, b, vars = c("x", "y"), simplify = TRUE)

plotEqn(A, b, vars = c("x", "y"))
        
# try to change the labels: doesn't work
plotEqn(A, b, vars = c("x", "y"),
        labels = c("y = x - 2",
                   "y = 1/2 - x",
                   "y = 1"))

# bring out code to find intersections to a function

intersections <- function(A, b) {
  neq <- nrow(A)
  res <- matrix(NA, nrow=neq*(neq - 1)/2, ncol=2)
  colnames(res) <- c("x", "y")
  k <- 0
  for (i in 1:(neq - 1)) {
    for (j in (i + 1):neq) {
      k <- k + 1
      x <- try(solve(A[c(i, j), ], b[c(i, j)]), silent=TRUE)
      if (!inherits(x, "try-error")) res[k, ] <- x
    }
  }
  res
}

intersections(A,b)
