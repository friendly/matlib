## ----nomessages, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  echo = TRUE,
  collapse = TRUE,
  fig.height = 5,
  fig.width = 5
)
options(digits=4)
par(mar=c(4,4,1,1)+.1)

## ----setuprgl, echo=FALSE-----------------------------------------------------
library(rgl)
library(knitr)
knit_hooks$set(webgl = hook_webgl)

## -----------------------------------------------------------------------------
library(matlib)   # use the package
library(rgl)      # also using rgl

## -----------------------------------------------------------------------------
x <- c(1, 1, -1, -1)
y <- 1:4

## ----plotmod1-----------------------------------------------------------------
(mod <- lm(y ~ x))

## ----plotmod2-----------------------------------------------------------------
par(mar=c(4,4,1,1)+.1)
plot(y ~ x, pch=16, cex=1.5)
abline(mod, lwd=2)
abline(h = coef(mod)[1], col="grey")

## ----printmat-----------------------------------------------------------------
X <- cbind(1, x)
printMatEqn(y, "=", X, "*", vec(c("b0", "b1")), "+", vec(paste0("e", 1:4)))


## -----------------------------------------------------------------------------
showEqn(X, y, vars=c("b0", "b1"), simplify=TRUE)

## ----ploteqn1, echo=2---------------------------------------------------------
par(mar=c(4,4,1,1)+.1)
plotEqn(X, y, vars=c("b0", "b1"), xlim=c(-2, 4))

## ----ploteqn2, echo=-1--------------------------------------------------------
par(mar=c(4,4,1,1)+.1)
plotEqn(X, y, vars=c("b0", "b1"), xlim=c(-2, 4))
solution <- lm( y ~ 0 + X)
loc <- coef(solution)
points(x=loc[1], y=loc[2], pch=16, cex=1.5)

## -----------------------------------------------------------------------------
O  <- c(0, 0, 0)    # origin
x0 <- J(3)          # intercept
x1 <- c(0, 1, -1)   # x
y  <- c(1, 1, 4)    # y
y <-  2 * y / floor(len(y))  # make length more convenient for 3D plot

## -----------------------------------------------------------------------------
X <- cbind(x0, x1)      # make a matrix
showEqn(X, y, vars=colnames(X), simplify=TRUE)

## ----plot3d, webgl=TRUE-------------------------------------------------------
win <- rgl::open3d()
# (1) draw observation axes
E <- diag(3)
rownames(E) <- c("1", "2", "3")
vectors3d(E, lwd=2, color="blue")

# (2) draw variable vectors
vectors3d(t(X), lwd=2, headlength=0.07)
vectors3d(y, labels=c("", "y"), color="red", lwd=3, headlength=0.07)

# (3) draw the plane spanned by x0, x1 
normal <- xprod(x0, x1)
rgl::planes3d(normal, col="turquoise", alpha=0.2)

# (4) draw projection of y on X
py <-  Proj(y, X)
rgl::segments3d(rbind( y, py))       # draw y to plane
rgl::segments3d(rbind( O, py))       # origin to py in the plane
corner( O, py, y, d=0.15)            # show it's a right angle
arc(y, O, py, d=0.2, color="red")


## ----eval=FALSE---------------------------------------------------------------
#  play3d(spin3d())

## -----------------------------------------------------------------------------
par(mar=c(4,4,1,1)+.1)
X <- cbind(x0, x1)
plotEqn(X, y, vars=c("b0", "b1"), xlim=c(-2, 4))
solution <- lm( y ~ 0 + X)
loc <- coef(solution)
points(x=loc[1], y=loc[2], pch=16, cex=1.5)
abline(v=loc[1], lty=2)
abline(h=loc[2], lty=2)


