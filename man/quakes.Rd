\name{quakes}
\alias{quakes}
\docType{data}
\title{Earthquakes over a 7 day period}
\description{
 This is a data frame giving the locations, magnitude, and depth of 
 earthquakes that occurred in a 7 day period (2008 Sep 14-21).  
 The data are from the USGS. 
}
\usage{data(quakes)}
\format{
  A data frame with 1074 observations on the following 7 variables.
  \describe{
    \item{\code{magnitude}}{a numeric vector}
    \item{\code{longitude}}{a numeric vector}
    \item{\code{latitude}}{a numeric vector}
    \item{\code{year}}{an integer vector - 2008}
    \item{\code{month}}{an integer vector - 9}
    \item{\code{day}}{a numeric vector - 14 to 21}
    \item{\code{depth}}{a numeric vector}
  }
}
\source{
http://earthquake.usgs.gov/earthquakes/catalogs/merged_catalog.xml.gz 
}

\examples{
data(quakes)
}
\keyword{datasets}
