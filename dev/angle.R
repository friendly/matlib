# Generalize the angle function to allow angle(X, Y), where X, Y are matrices
# and we want to calculate the angles between the columns of X, Y.
# Or, angle(X), and want to calculate the angles between the columns of X

angle <- function(x, y, degree = TRUE) {
  if(missing(y)) y <- x
  if(is.vector(x) && is.atomic(x)) {
    theta <- acos(x %*% y / (len(x) * len(y)))
  }
  else {
    theta <- acos(t(x) %*% y / outer(len(x), len(y)))
  }
  if(degree) theta <- r2d(theta)
  theta
}

if(FALSE) {
# one or two vectors
x <- c(-2,1)
y <- c(1,1)
angle(x, y) # degrees
angle(x, y, degree = FALSE) # radians
angle(x)    # for one arg case

# matrices

X <- matrix(
      c(1, 0, 1, 1, -2, 1),
      ncol = 2,
      byrow = TRUE)

Y <- matrix(
      c(1, 1, -1, -1),
      ncol = 2,
      byrow = TRUE)

angle(X, Y)
angle(X)

}
