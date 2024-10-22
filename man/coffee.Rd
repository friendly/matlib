\name{coffee}
\alias{coffee}
\docType{data}
\title{
Data on Coffee, Stress and Heart Damage
}
\description{
A dataset, used for examples in Friendly, Fox & Monette (2013),
giving an index of cardiac damage (\code{Heart})
in relation to a measure of daily coffee consumption (\code{Coffee})
and \code{Stress}, a measure of perceived occupational stress,
in a contrived sample of \eqn{n = 20} university people.
}

\usage{data("coffee")}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{Group}}{university group, a factor with levels \code{Grad_Student} \code{Professor} \code{Student}}
    \item{\code{Coffee}}{a measure of daily coffee consumption}
    \item{\code{Stress}}{a measure of perceived occupational stress}
    \item{\code{Heart}}{an index of cardiac damage}
  }
}
\details{
The main goal of analysis would be
to determine whether or not coffee is good or bad for your heart, and stress
represents one potential confounding variable among others (age, smoking, etc.)
that might be useful to control statistically.

}
\source{
This dataset was constructed by Georges Monette, and was modified from that in his \code{spida2} package.
}
\references{
Friendly, M., Monette, G., & Fox, J. (2013). 
Elliptical Insights: Understanding Statistical Methods Through Elliptical Geometry. 
\emph{Statistical Science}, \bold{28}(1), 1–39. https://doi.org/10.1214/12-STS402
}
\examples{
data(coffee)
## maybe str(coffee) ; plot(coffee) ...
}
\keyword{datasets}
