---
  title: "Test plot math in vectors3d"
---
  
library(rgl)
library(matlib)

# want to use labels that are expressions.
# text3d() now allows this, using `plotmath3d()` for expressions
labs <- c(expression(x[1]), "y", expression(x[2]))
is.expression(labs)

source(here::here("dev", "vectors3d.R"))

open3d()
E <- diag(3)
rownames(E) <- c(expression(x[1]), "y", expression(x[2]))
vectors3d(E, lwd=2)
vectors3d(c(1, 1, 1),
          labels=c(expression(hat(y))), color="red",
          lwd=3)
vectors3d(c(1, 1, 0),
          labels=c("", "x+y"),
          color="green", lwd=2)
planes3d(0, 0, 1, 0, col="gray",
         alpha=0.1)
segments3d(rbind(c(1, 1, 1),
                 c(1, 1, 0)))
arc(c(1, 1, 1), c(0, 0, 0),
    c(1, 1, 0))
corner(c(0, 0, 0), c(1, 1, 0), c(1, 1, 1))


