\name{housing}
\alias{housing}
\docType{data}
\title{Housing Sales in the San Francisco Bay Area}
\description{
  Information about houses sold in the SF Bay Area between Apr 2003 and May 2006.
}
\usage{data(housing)}
\format{
  A data frame with 281506 observations on the following 15 variables.
  \describe{
    \item{\code{county}}{County name; factor}
    \item{\code{city}}{City name; factor}
    \item{\code{zip}}{Postal code; factor}
    \item{\code{street}}{Street Address; character}
    \item{\code{price}}{Sale price; numeric}
    \item{\code{br}}{Number bedrooms; integer}
    \item{\code{lsqft}}{lot size (feet^2); numeric}
    \item{\code{bsqft}}{building size (feet^2); numeric}
    \item{\code{year}}{year previous sale; numeric}
    \item{\code{date}}{date sold; POSIXt}
    \item{\code{long}}{logitude; numeric}
    \item{\code{lat}}{latitude; numeric}
    \item{\code{quality}}{factor}
    \item{\code{match}}{Type of match; factor}
    \item{\code{wk}}{week of sale; Date}
  }
}
\source{
http://sfgate.com

https://github.com/hadley/sfhousing

Hadley Wickham, Deborah Swayne, and David Poole. O'Reilly & Associates, Inc. 
Bay Area Blues: The Effect of the Housing Crisis. 303-322. 
In Beautiful Data: The Stories Behind Elegant Data Solutions. 
Toby Segaran.  Jeff Hammerbacher. 2009. 
}

\examples{
data(housing)
}
\keyword{datasets}
