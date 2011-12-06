\name{kmlText}
\alias{kmlText}
\alias{text,KMLDoc-method}
\title{Display string via KML}
\description{
  This displays a collection of strings at the specified
  (longitude, latitude) locations via KML.

  This does not support styles, colors, fonts, etc.
}
\usage{
kmlText(long, lat, labels, ..., icon = system.file("Icons", "empty.png", package = "RKML"), parent = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long,lat}{vectors of latitude and longitudes at which the
     text is to be displayed.}
  \item{labels}{a character vector giving the string to be displayed at
    each of the (long, lat) coordiantes}
  \item{\dots}{additional parameters}
  \item{icon}{the icon used for the Placemark node }
  \item{parent}{the internal XML node that is to act as the parent node
    for the newly created Placemark nodes.}
}
\value{
A list of new \code{Placemark} nodes.
}
\references{
\url{http://code.google.com/apis/kml/documentation/kmlreference.html}
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{kmlSegments}}
  \code{\link{kmlCircle}}
}
\examples{
}
\keyword{graphics}
\keyword{aplot}
