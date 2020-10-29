## ----nomessages, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE
)
options(digits=4)
par(mar=c(5,4,1,1)+.1)

## -----------------------------------------------------------------------------
library(matlib)

## -----------------------------------------------------------------------------
   A <- matrix( c(1, 2, 3,
                  2, 3, 0,
                  0, 1,-2), nrow=3, byrow=TRUE)

## -----------------------------------------------------------------------------
   (AI <-  cbind(A, diag(3)))

## -----------------------------------------------------------------------------
	(AI[2,] <- AI[2,] - 2*AI[1,])     # row 2 <- row 2 - 2 * row 1
	(AI[3,] <- AI[3,] + AI[2,])       # row 3 <- row 3 + row 2
	(AI[2,] <- -1 * AI[2,])           # row 2 <- -1 * row 2
	(AI[3,] <-  -(1/8) * AI[3,])        # row 3 <- -.25 * row 3

## -----------------------------------------------------------------------------
	AI

      #--continue, making above diagonal == 0
	AI[2,] <- AI[2,] - 6 * AI[3,]     # row 2 <- row 2 - 6 * row 3
	AI[1,] <- AI[1,] - 3 * AI[3,]     # row 1 <- row 1 - 3 * row 3
	AI[1,] <- AI[1,] - 2 * AI[2,]     # row 1 <- row 1 - 2 * row 2

	AI
   #-- last three cols are the inverse
  (AInv <- AI[,-(1:3)])

   #-- compare with inv()
  inv(A)

## -----------------------------------------------------------------------------
   AI <-  cbind(A, diag(3))

   AI <- rowadd(AI, 1, 2, -2)        # row 2 <- row 2 - 2 * row 1
   AI <- rowadd(AI, 2, 3, 1)         # row 3 <- row 3 + row 2
   AI <- rowmult(AI, 2, -1)          # row 1 <- -1 * row 2
   AI <- rowmult(AI, 3, -1/8)        # row 3 <- -.25 * row 3

   # show result so far
   AI

  	#--continue, making above-diagonal == 0
   AI <- rowadd(AI, 3, 2, -6)        # row 2 <- row 2 - 6 * row 3
   AI <- rowadd(AI, 2, 1, -2)        # row 1 <- row 1 - 2 * row 2
   AI <- rowadd(AI, 3, 1, -3)        # row 1 <- row 1 - 3 * row 3
   AI

## -----------------------------------------------------------------------------
   echelon( cbind(A, diag(3)))

## -----------------------------------------------------------------------------
   echelon( cbind(A, diag(3)), verbose=TRUE, fractions=TRUE)

