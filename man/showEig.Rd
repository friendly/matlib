% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/showEig.R
\name{showEig}
\alias{showEig}
\title{Show the eigenvectors associated with a covariance matrix}
\usage{
showEig(
  X,
  col.vec = "blue",
  lwd.vec = 3,
  mult = sqrt(qchisq(levels, 2)),
  asp = 1,
  levels = c(0.5, 0.95),
  plot.points = TRUE,
  add = !plot.points,
  ...
)
}
\arguments{
\item{X}{A two-column matrix or data frame}

\item{col.vec}{color for eigenvectors}

\item{lwd.vec}{line width for eigenvectors}

\item{mult}{length multiplier(s) for eigenvectors}

\item{asp}{aspect ratio of plot, set to \code{asp=1} by default, and passed to dataEllipse}

\item{levels}{passed to dataEllipse determining the coverage of the data ellipse(s)}

\item{plot.points}{logical; should the points be plotted?}

\item{add}{logical; should this call add to an existing plot?}

\item{...}{other arguments passed to \code{\link[car]{dataEllipse}}}
}
\description{
This function is designed for illustrating the eigenvectors associated with the
covariance matrix for a given bivariate data set.  It draws a data ellipse of
the data and adds vectors showing the eigenvectors of the covariance matrix.
}
\examples{
x <- rnorm(200)
y <- .5 * x + .5 * rnorm(200)
X <- cbind(x,y)
showEig(X)

# Duncan data
data(Duncan, package="carData")
showEig(Duncan[, 2:3], levels=0.68)
showEig(Duncan[,2:3], levels=0.68, robust=TRUE, add=TRUE, fill=TRUE)
}
\seealso{
\code{\link[car]{dataEllipse}}
}
\author{
Michael Friendly
}
