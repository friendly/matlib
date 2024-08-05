# write out linear equations symbolically

A <- matrix(paste0("a_{", outer(1:3, 1:3, FUN  = paste0), "}"),
            nrow=3)
x <- paste0("x_", 1:3)
b <- paste0("b_", 1:3)
showEqn(A, b, vars = x)


A <- matrix(paste0("a_{", outer(1:3, 1:3, FUN  = paste0), "}"),
            nrow=3)
b <- paste0("b_", 1:3)
x <- paste0("x", 1:3)
showEqn(A, b, vars = x, latex=TRUE)

