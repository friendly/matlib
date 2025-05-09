# show dual relation between points & lines

library(matlib)

source(here::here("dev", "plotEqn.R"))

A <- matrix(c( 1, 2, 0,
               -1, 2, 1), 3, 2) |> print()

b <- c(2, 1, 1)

showEqn(A, b, vars = c("x", "y"), simplify = TRUE)

plotEqn(A, b, vars = c("x", "y"))
        
# try to change the labels: doesn't work
plotEqn(A, b, vars = c("x", "y"),
        labels = c("y = x - 2",
                   "y = 1/2 - x",
                   "y = 1"))
