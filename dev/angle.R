# Generalize the angle function to allow angle(X, Y), where X, Y are matrices
# and we want to calculate the angles between the columns of X, Y.
# Or, angle(X), and want to calculate the angles between the columns of X
# see: https://stackoverflow.com/questions/78827317/find-angles-between-columns-of-matrices-in-r


rad2deg <- function(rad) (rad * 180) / pi

angle <- function(x, y, degree = TRUE) {
  if(missing(y)) y <- x
  if(is.vector(x) && is.atomic(x)) {
    theta <- acos(x %*% y / (len(x) * len(y)))

    if(degree) theta <- rad2deg(theta)
    return(theta)
  }
  else {
    outer(
      asplit(x, 2),
      asplit(y, 2),
      Vectorize(\(x, y) angle(c(x), c(y), degree=degree))
    )  }
}



# angle <- function(x, y, degree = TRUE) {
#   if(missing(y)) y <- x
#   if(is.vector(x) && is.atomic(x)) {
#     theta <- acos(x %*% y / (len(x) * len(y)))
#   }
#   else {
#     theta <- acos(t(x) %*% y / outer(len(x), len(y)))
#   }
#   if(degree) theta <- r2d(theta)
#   theta
# }

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
      c(1, 1, -1, -1, 0, 1),
      ncol = 2,
      byrow = TRUE)

angle(X, Y)
angle(X)

outer(
  asplit(X, 2),
  asplit(Y, 2),
  Vectorize(\(x, y) angle(c(x), c(y), TRUE))
)

outer(
  asplit(X, 2),
  asplit(X, 2),
  Vectorize(\(x, y) angle(c(x), c(y), TRUE))
)



}
