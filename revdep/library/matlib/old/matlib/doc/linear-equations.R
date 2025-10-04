## ----nomessages, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE
)
options(digits=4)
par(mar=c(5,4,1,1)+.1)

## ----setuprgl, echo=FALSE-----------------------------------------------------
library(rgl)
library(knitr)
knit_hooks$set(webgl = hook_webgl)

## ----include=FALSE------------------------------------------------------------
library(matlib)   # use the package

## ----showEqn0, results='asis'-------------------------------------------------
A <- matrix(paste0("a_{", outer(1:3, 1:3, FUN  = paste0), "}"), 
            nrow=3) 
b <- paste0("b_", 1:3)
x <- paste0("x", 1:3)
showEqn(A, b, vars = x, latex=TRUE)

## -----------------------------------------------------------------------------
library(matlib)   # use the package

## ----consistent---------------------------------------------------------------
A <- matrix(c(1, 2, -1, 2), 2, 2)
b <- c(2,1)
showEqn(A, b)

## ----check-consistent---------------------------------------------------------
c( R(A), R(cbind(A,b)) )          # show ranks
all.equal( R(A), R(cbind(A,b)) )  # consistent?

## ----plotEqn1,echo=2----------------------------------------------------------
par(mar=c(4,4,0,0)+.1)
plotEqn(A,b)

## ----Solve--------------------------------------------------------------------
Solve(A, b, fractions = TRUE)

## ----showEqn------------------------------------------------------------------
A <- matrix(c(1,2,3, -1, 2, 1), 3, 2)
b <- c(2,1,3)
showEqn(A, b)
c( R(A), R(cbind(A,b)) )          # show ranks
all.equal( R(A), R(cbind(A,b)) )  # consistent?

Solve(A, b, fractions=TRUE)       # show solution 

## ----plotEqn2,echo=2----------------------------------------------------------
par(mar=c(4,4,0,0)+.1)
plotEqn(A,b)

## ----showEqn2-----------------------------------------------------------------
A <- matrix(c(1,2,3, -1, 2, 1), 3, 2)
b <- c(2,1,6)
showEqn(A, b)
c( R(A), R(cbind(A,b)) )          # show ranks
all.equal( R(A), R(cbind(A,b)) )  # consistent?

## ----echelon------------------------------------------------------------------
echelon(A, b)

## -----------------------------------------------------------------------------
Solve(A, b, fractions=TRUE)

## ----ginv---------------------------------------------------------------------
x <- MASS::ginv(A) %*% b
x

## ----plotEqn4-----------------------------------------------------------------
par(mar=c(4,4,0,0)+.1)
plotEqn(A,b, xlim=c(-2, 4))
# add the ginv() solution
points(x[1], x[2], pch=15)

## ----three-eqn----------------------------------------------------------------
A <- matrix(c(2, 1, -1,
             -3, -1, 2,
             -2,  1, 2), 3, 3, byrow=TRUE)
colnames(A) <- paste0('x', 1:3)
b <- c(8, -11, -3)
showEqn(A, b)

## -----------------------------------------------------------------------------
c( R(A), R(cbind(A,b)) )          # show ranks
all.equal( R(A), R(cbind(A,b)) )  # consistent?

## -----------------------------------------------------------------------------
solve(A, b)

## -----------------------------------------------------------------------------
solve(A) %*% b
inv(A) %*% b

## -----------------------------------------------------------------------------
echelon(A, b)

## -----------------------------------------------------------------------------
echelon(A, b, verbose=TRUE, fractions=TRUE)

## ----plotEqn3, webgl=TRUE-----------------------------------------------------
plotEqn3d(A,b, xlim=c(0,4), ylim=c(0,4))

## -----------------------------------------------------------------------------
A <- matrix(c(1,  3, 1,
              1, -2, -2,
              2,  1, -1), 3, 3, byrow=TRUE)
colnames(A) <- paste0('x', 1:3)
b <- c(2, 3, 6)
showEqn(A, b)

## -----------------------------------------------------------------------------
c( R(A), R(cbind(A,b)) )          # show ranks
all.equal( R(A), R(cbind(A,b)) )  # consistent?

