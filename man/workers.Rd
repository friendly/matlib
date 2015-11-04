\name{workers}
\alias{workers}
\docType{data}
\title{
Workers Data
}
\description{
A toy data set comprised of information on workers \code{Income} in relation
to other variables, used for illustrating linear and multiple regression.
}
\usage{data("workers")}
\format{
  A data frame with 10 observations on the following 4 variables.
  \describe{
    \item{\code{Income}}{income from the job, a numeric vector}
    \item{\code{Experience}}{number of years of experience, a numeric vector}
    \item{\code{Skill}}{skill level in the job, a numeric vector}
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
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
data(workers)
plot(Income ~ Experience, data=workers, main="Income ~ Experience", pch=20, cex=2)

# simple linear regression
reg1 <- lm(Income ~ Experience, data=workers)
abline(reg1, col="red", lwd=3)

# quadratic fit?
plot(Income ~ Experience, data=workers, main="Income ~ poly(Experience,2)", pch=20, cex=2)
reg2 <- lm(Income ~ poly(Experience,2), data=workers)
fit2 <-predict(reg2)
abline(reg1, col="red", lwd=1, lty=1)
lines(workers$Experience, fit2, col="blue", lwd=3)

# How does Income depend on a factor?
plot(Income ~ Gender, data=workers, main="Income ~ Gender")
points(workers$Gender, jitter(workers$Income), cex=2, pch=20)
means<-aggregate(workers$Income,list(workers$Gender),mean)
points(means,col="red", pch="+", cex=2)
lines(means,col="red", lwd=2)

}
\keyword{datasets}
