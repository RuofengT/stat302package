% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_lm.R
\name{my_lm}
\alias{my_lm}
\title{Linear Regression}
\usage{
my_lm(formula, data)
}
\arguments{
\item{formula}{Formula used for regression}

\item{data}{A dataset used for regression}
}
\value{
The estimate, the standard error, the t value, and Pr(>|t|) for
each coefficient including the intercept
}
\description{
This function performs linear regression on given data
}
\examples{
my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)

}
\keyword{inference}
\keyword{prediction}
