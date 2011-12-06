\name{kmlPolygon}
\alias{kmlPolygon}
\title{Display a polygon in KML}
\description{
  This function creates a display for a polygon in KML given the
}
\usage{
kmlPolygon(long, lat, col = character(), fill = FALSE, lwd = 1,
            outline = 1,
            style = list(PolyStyle = list(color = col, fill = fill,
                                          width = lwd, outline = outline)),
            parent = NULL, localStyle = missing(style) || !is.character(style))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long,lat}{numeric vectors giving the longitude and latitude of
    the points to connect in the }
  \item{col}{the color for the }
  \item{fill}{a logical value indicating whether to fill the interior of
  the polygon (with col as the color?)}
  \item{lwd}{the line width to use for the outline of the polygon}
  \item{outline}{a logical value that provides the value for the outline
  element in the PolyStyle style object in KML}
  \item{style}{a description of the KML style to use. This can be list
    in R that is used to create a new Style element in KML, or a
    character string giving the name of a top-level Style.}
  \item{parent}{an XML internal node used as the parent node of the new
    Placemark XML node used to display this polygon.}
  \item{localStyle}{a logical value that controls whether to create the
    style locally or as a top-level style within the KML document.}
}
\details{
}
\value{
  An XMLInternalNode giving the Placemark that contains the polygon description.
}
\references{
\url{http://code.google.com/apis/kml/documentation/kmlreference.html}
}
\author{
Duncan Temple Lang
}
\seealso{
}
\examples{
}
\keyword{graphics}
\keyword{aplot}


