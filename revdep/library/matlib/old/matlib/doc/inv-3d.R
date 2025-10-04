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

## ----unit-cube----------------------------------------------------------------
library(rgl)
library(matlib)
# cube, with each face colored differently
colors <- rep(2:7, each=4)
c3d <- cube3d()
# make it a unit cube at the origin
c3d <- scale3d(translate3d(c3d, 1, 1, 1),
               .5, .5, .5)
str(c3d)

## ----vertices-----------------------------------------------------------------
c3d$vb

## ----matrix-A-----------------------------------------------------------------
# matrix A: 
A <- matrix(c( 1, 0, 1, 
               0, 2, 0,  
               1, 0, 2), 3, 3) |> print()
det(A)

# A can be produced by elementary row operations on the identity matrix
I <- diag( 3 )

AA <- I |>
  rowadd(3, 1, 1) |>   # add 1 x row 3 to row 1
  rowadd(1, 3, 1) |>   # add 1 x row 1 to row 3
  rowmult(2, 2)        # multiply row 2 by 2

all(AA==A)

## ----draw3d-------------------------------------------------------------------
# draw a mesh3d object with vertex points and lines
draw3d <- function(object, col=rep(rainbow(6), each=4), 
                   alpha=0.6,  
                   vertices=TRUE, 
                   lines=TRUE, 
                   reverse = FALSE,
                   ...) {
  if(reverse) col <- rev(col)
	shade3d(object, col=col, alpha=alpha, ...)
	vert <- t(object$vb)
	indices <- object$ib
	if (vertices) points3d(vert, size=5)
	if (lines) {
	for (i in 1:ncol(indices))
		lines3d(vert[indices[,i],])
		}
}

# label vertex points
vlabels <- function(object, vertices, labels=vertices, ...) {
	text3d( t(object$vb[1:3, vertices] * 1.05), texts=labels, ...)
}

## ----3d-demo1, webgl=TRUE-----------------------------------------------------
open3d()
draw3d(c3d)
vlabels(c3d, c(1,2,3,5))

axes <- rbind( diag(3), -diag(3) )
rownames(axes) <- c("x", "y", "z", rep(" ", 3))
vectors3d(axes, frac.lab=1.2, headlength = 0.2, radius=1/20, lwd=3)

c3t<- transform3d(c3d, A) 
draw3d(c3t)
vlabels(c3t, c(1,2,3,5))

## ----3d-demo2, webgl=TRUE-----------------------------------------------------
# NB: this scales each one separately, so can't see relative size
open3d()
mfrow3d(1,2, sharedMouse=TRUE)
draw3d(c3d)
vectors3d(axes, frac.lab=1.2, headlength = 0.2, radius=1/20, lwd=3)

next3d()	
draw3d(c3t)
vectors3d(axes, frac.lab=1.2, headlength = 0.2, radius=1/20, lwd=3)

## ----AI-----------------------------------------------------------------------
AI <- solve(A) |> print()

## ----det-AI-------------------------------------------------------------------
det(A)
det(AI)

## ----3d-demo3, webgl=TRUE-----------------------------------------------------
open3d()
draw3d(c3t)
c3Inv <- transform3d(c3d, solve(A))
draw3d(c3Inv)
vectors3d(axes, frac.lab=1.2, headlength = 0.2, radius=1/20, lwd=3)
vlabels(c3t, 8, "A", cex=1.5)
vlabels(c3t, c(2,3,5))

vlabels(c3Inv, 4, "Inv", cex=1.5)
vlabels(c3Inv, c(2,3,5))

## ----3d-demo4, webgl=TRUE-----------------------------------------------------
play3d(spin3d(rpm=15),  duration=4)

#movie3d(spin3d(rpm=15), duration=4, movie="inv-demo", dir=".")

