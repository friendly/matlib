## ----nomessages, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE
)
options(digits=4)
par(mar=c(5,4,1,1)+.1)

## ----class1-------------------------------------------------------------------
library(matlib)
data(class)
class$male <- as.numeric(class$sex=="M")

## ----class2-------------------------------------------------------------------
class <- transform(class, 
                   IQ = round(20 + height + 3*age -.1*weight -3*male + 10*rnorm(nrow(class))))
head(class)

## ----class3-------------------------------------------------------------------
X <- as.matrix(class[,c(3,4,2,5)])
head(X)

## -----------------------------------------------------------------------------
Z <- cbind(X[,1], 0, 0, 0)
Z[,2] <- X[,2] - Proj(X[,2], Z[,1])
crossprod(Z[,1], Z[,2])     # verify orthogonality

## -----------------------------------------------------------------------------
Z[,3] <- X[,3] - Proj(X[,3], Z[,1]) - Proj(X[,3], Z[,2]) 
Z[,4] <- X[,4] - Proj(X[,4], Z[,1]) - Proj(X[,4], Z[,2]) - Proj(X[,4], Z[,3])

## ----usinglm------------------------------------------------------------------
z2 <- residuals(lm(X[,2] ~ X[,1]), type="response")
z3 <- residuals(lm(X[,3] ~ X[,1:2]), type="response")
z4 <- residuals(lm(X[,4] ~ X[,1:3]), type="response")

## ----ortho1-------------------------------------------------------------------
zapsmall(crossprod(Z))     # check orthogonality

## ----ortho2-------------------------------------------------------------------
Z <- Z %*% diag(1 / len(Z))    # make each column unit length
zapsmall(crossprod(Z))         # check orthonormal
colnames(Z) <- colnames(X)

## ----QR-----------------------------------------------------------------------
# same result as QR(X)$Q, but with signs reversed
head(Z, 5)
head(-QR(X)$Q, 5)
all.equal( unname(Z), -QR(X)$Q )

## ----class2IQ-----------------------------------------------------------------
class2 <- data.frame(Z, IQ=class$IQ)

## ----mod1, R.options=list(digits=5)-------------------------------------------
mod1 <- lm(IQ ~ height + weight + age + male, data=class)
anova(mod1)

## ----mod2, R.options=list(digits=5)-------------------------------------------
mod2 <- lm(IQ ~ height + weight + age + male, data=class2)
anova(mod2)

