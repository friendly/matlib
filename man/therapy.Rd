\name{therapy}
\alias{therapy}
\docType{data}
\title{
Therapy Data
}
\description{
A toy data set on outcome in \code{therapy} in relation to a personality test (\code{perstest})
and a scale of internal-external locus of control (\code{IE})
used to illustrate linear and multiple regression.
}
\usage{data("therapy")}
\format{
  A data frame with 10 observations on the following 4 variables.
  \describe{
    \item{\code{sex}}{a factor with levels \code{F} \code{M}}
    \item{\code{perstest}}{score on a personality test, a numeric vector}
    \item{\code{therapy}}{outcome in psychotherapy, a numeric vector}
    \item{\code{IE}}{score on a scale of internal-external locus of control, a numeric vector}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(therapy)
plot(therapy ~ perstest, data=therapy, pch=16)
abline(lm(therapy ~ perstest, data=therapy), col="red")

plot(therapy ~ perstest, data=therapy, cex=1.5, pch=16, 
	col=ifelse(sex=="M", "red","blue"))
}
\keyword{datasets}
