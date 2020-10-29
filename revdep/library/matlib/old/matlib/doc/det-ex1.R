## ----nomessages, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.height = 5,
  fig.width = 5
)
options(digits=4)
par(mar=c(3,3,1,1)+.1)

## -----------------------------------------------------------------------------
A <- matrix(c(3, 1, 
              2, 4), nrow=2, byrow=TRUE)
A
det(A)

## -----------------------------------------------------------------------------
det(A[ 2:1, ])
det(A[, 2:1 ])

## -----------------------------------------------------------------------------
det( t(A) )

## -----------------------------------------------------------------------------
diag(c(3, 1)) %*% A
det( diag(c(3, 1))  %*% A)

## -----------------------------------------------------------------------------
det(3 * A)
3^2 * det(A)

## -----------------------------------------------------------------------------
B <- matrix(c(4, 2, 
             3, 5), nrow=2, byrow=TRUE)
B
det(A %*% B)
det(A) * det(B)

## -----------------------------------------------------------------------------
C <- matrix(c(1, 5, 
             2, 6,
             4, 4), nrow=3, byrow=TRUE)
C <- cbind(C, C[,1])
C
det(C)

## -----------------------------------------------------------------------------
A[2,] <- A[2,] - 2*A[1,]
det(A)

## -----------------------------------------------------------------------------
A <- matrix(c(3, 1, 
              2, 4), nrow=2, byrow=TRUE)
A
det(A)

## ----fig.width=5, fig.height=5------------------------------------------------
library(matlib)
xlim <- c(0,6)
ylim <- c(0,6)
par(mar=c(3,3,1,1)+.1)
plot(xlim, ylim, type="n", xlab="X1", ylab="X2", asp=1)
sum <- A[1,] + A[2,]
# draw the parallelogram determined by the rows of A
polygon( rbind(c(0,0), A[1,], sum, A[2,]), col=rgb(1,0,0,.2))
vectors(A, labels=c("a1", "a2"), pos.lab=c(4,2))
vectors(sum, origin=A[1,], col="gray")
vectors(sum, origin=A[2,], col="gray")
# add some annotations
text(0,6, "det(A) is the area of its row vectors", pos=4)
text(mean(A[,1]), mean(A[,2]), "det(A)", cex=1.25)


## -----------------------------------------------------------------------------
(D <- 2 * diag(2))
det(D)

## ----fig.width=4, fig.height=4------------------------------------------------
par(mar=c(3,3,1,1)+.1)
plot(c(0,2), c(0,2), type="n", xlab="X1", ylab="X2", asp=1)
sum <- D[1,] + D[2,]
polygon( rbind(c(0,0), D[1,], sum, D[2,]), col=rgb(0,1,0,.2))
vectors(D, labels=c("d1", "d2"), pos.lab=c(3,4))
vectors(sum, origin=D[1,], col="gray")
vectors(sum, origin=D[2,], col="gray")
text(mean(D[,1]), mean(D[,2]), "det(D)", cex=1.25)

## -----------------------------------------------------------------------------
(B <- matrix(c(1, 2, 2, 4), 2,2))
det(B)

## ----fig.width=4, fig.height=4------------------------------------------------
par(mar=c(3,3,1,1)+.1)
plot(c(0,4), c(0,4), type="n", xlab="X1", ylab="X2", asp=1)
vectors(B, labels=c("b1", "b2"), pos.lab=c(4,2))


