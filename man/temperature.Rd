\name{temperature}
\alias{temperature}
\docType{data}
\title{Temperatures for 100 US cities in each of 4 months.}
\description{
  This is a data frame with average temperatures for
  each of 4 months -  Jan, April, July and October -
  for one hundred cities, along with their longitude and latitude.
  
}
\usage{data(temperature)}
\format{
  A data frame with 400 observations on the following 6 variables.
  \describe{
    \item{\code{temp}}{a numeric vector}
    \item{\code{month}}{an ordered factor with levels \code{January} < \code{April} < \code{July} < \code{October}}
    \item{\code{city}}{a character vector}
    \item{\code{latitude}}{a numeric vector}
    \item{\code{longitude}}{a numeric vector}
    \item{\code{state}}{a character vector}
  }
}
\source{
 Can't remember.
}
\examples{
data(temperature)
}
\keyword{datasets}
