## ----nomessages, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.height = 5,
  fig.width = 5
)
options(digits=4)
par(mar=c(5,4,1,1)+.1)

## -----------------------------------------------------------------------------
library(matlib)

## -----------------------------------------------------------------------------
    A <- matrix( c(5, 1, 0,
                   3,-1, 2,
                   4, 0,-1), nrow=3, byrow=TRUE)
   det(A)

## -----------------------------------------------------------------------------
   (AI  <- inv(A))

## -----------------------------------------------------------------------------
   AI %*% A

## -----------------------------------------------------------------------------
   inv(AI)

## -----------------------------------------------------------------------------
   inv( t(A) )
   is_symmetric_matrix(A)
   is_symmetric_matrix( inv( t(A) ) )

## -----------------------------------------------------------------------------
   B <- matrix( c(4, 2, 2,
                  2, 3, 1,
                  2, 1, 3), nrow=3, byrow=TRUE)
   inv(B)
   inv( t(B) )
   is_symmetric_matrix(B)
   is_symmetric_matrix( inv( t(B) ) )
   all.equal( inv(B), inv( t(B) ) )

## -----------------------------------------------------------------------------
   D <- diag(c(1, 2, 4))
   inv(D)
   MASS::fractions( diag(1 / c(1, 2, 4)) )

## -----------------------------------------------------------------------------
   A <- matrix(c(1, 2, 3,  2, 3, 0,  0, 1, 2), nrow=3, byrow=TRUE)
   AI <- inv(A)
   inv(AI)

## -----------------------------------------------------------------------------
   inv( t(A) )
   t( inv(A) )

## -----------------------------------------------------------------------------
   inv(5 * A)
   (1/5) * inv(A)

## -----------------------------------------------------------------------------
   B <- matrix(c(1, 2, 3, 1, 3, 2, 2, 4, 1), nrow=3, byrow=TRUE)
   C <- B[, 3:1]
   A %*%  B
   inv(A %*%  B)

   inv(B) %*%  inv(A)

## -----------------------------------------------------------------------------
   (ABC <- A %*% B %*% C)
   inv(A %*% B %*% C)
   inv(C) %*% inv(B) %*% inv(A)
   inv(ABC)

## -----------------------------------------------------------------------------
  det(AI)
  1 / det(A)

## -----------------------------------------------------------------------------
A <- matrix(c(2, 1, 
              1, 2), nrow=2, byrow=TRUE)
A
det(A)

## -----------------------------------------------------------------------------
AI <- inv(A)
MASS::fractions(AI)
det(AI)

## ----plot-inv1----------------------------------------------------------------
par(mar=c(3,3,1,1)+.1)
xlim <- c(-1,3)
ylim <- c(-1,3)
plot(xlim, ylim, type="n", xlab="X1", ylab="X2", asp=1)
sum <- A[1,] + A[2,]
# draw the parallelogram determined by the rows of A
polygon( rbind(c(0,0), A[1,], sum, A[2,]), col=rgb(1,0,0,.2))
vectors(A, labels=c(expression(a[1]), expression(a[2])), pos.lab=c(4,2))
vectors(sum, origin=A[1,], col="gray")
vectors(sum, origin=A[2,], col="gray")
text(mean(A[,1]), mean(A[,2]), "A", cex=1.5)

## ----plot-inv2, echo=-(1:12)--------------------------------------------------
par(mar=c(3,3,1,1)+.1)
xlim <- c(-1,3)
ylim <- c(-1,3)
plot(xlim, ylim, type="n", xlab="X1", ylab="X2", asp=1)
sum <- A[1,] + A[2,]
# draw the parallelogram determined by the rows of A
polygon( rbind(c(0,0), A[1,], sum, A[2,]), col=rgb(1,0,0,.2))
vectors(A, labels=c(expression(a[1]), expression(a[2])), pos.lab=c(4,2))
vectors(sum, origin=A[1,], col="gray")
vectors(sum, origin=A[2,], col="gray")
text(mean(A[,1]), mean(A[,2]), "A", cex=1.5)

vectors(AI, labels=c(expression(a^1), expression(a^2)), pos.lab=c(4,2))
sum <- AI[1,] + AI[2,]
polygon( rbind(c(0,0), AI[1,], sum, AI[2,]), col=rgb(0,0,1,.2))
text(mean(AI[,1])-.3, mean(AI[,2])-.2, expression(A^{-1}), cex=1.5)

## -----------------------------------------------------------------------------
(A <- matrix(c(2, 1, 1, 1), nrow=2))
(AI <- inv(A))

## ----plot-inv3, echo=FALSE----------------------------------------------------
par(mar=c(3,3,1,1)+.1)
xlim <- c(-1,3)
ylim <- c(-1,3)
plot(xlim, ylim, type="n", xlab="X1", ylab="X2", asp=1)
sum <- A[1,] + A[2,]
# draw the parallelogram determined by the rows of A
polygon( rbind(c(0,0), A[1,], sum, A[2,]), col=rgb(1,0,0,.2))
vectors(A, labels=c(expression(a[1]), expression(a[2])), pos.lab=c(4,2))
vectors(sum, origin=A[1,], col="gray")
vectors(sum, origin=A[2,], col="gray")
text(mean(A[,1]), mean(A[,2]), "A", cex=1.5)

vectors(AI, labels=c(expression(a^1), expression(a^2)), pos.lab=c(4,2))
sum <- AI[1,] + AI[2,]
polygon( rbind(c(0,0), AI[1,], sum, AI[2,]), col=rgb(0,0,1,.2))
text(-.1, -.1, expression(A^{-1}), cex=1.5)

