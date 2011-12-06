\name{groundOverlay}
\alias{groundOverlay}
\title{Display an image on Google Earth}
\description{
  This function displays the specified image at a location
  on Google Earth/Maps. We might use this for example
  to overlay a rasterized model fit or a plot.
  (Alternatively, we can use the KML device to create
    R plots using KML elements.)
  
  This is different from displaying an image on the screen,
  e.g. for a legend. This places the image at the given
  longitude, latitude rather than relative to screen coordinates.
}
\usage{
groundOverlay(icon, long, lat, width, height, rot = 0, ..., name = character(), parent = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{icon}{the name of the file containing the image to be overlayed}
  \item{long,lat}{the longitude and latitude at which to locate the image}
  \item{width,height}{the width and height of the image in units of longitude and
  latitude.}
  \item{rot}{how much to rotate the image. This is an angle between 180
  and -180.}
  \item{\dots}{additional parameters. Currently ignored.}
  \item{name}{the name to use for the GroundOverlay KML element. This
      will be displayed in the Google Earth places list to identify the element. }
  \item{parent}{the parent XML node for the new GroundOverlay node. THis
  can be \code{NULL} in which case the caller must parent this
  appropriately after the call returns the node.} 
}
\value{
An XML internal node giving a \code{GroundOverlay} element.
}
\references{
\url{http://code.google.com/apis/kml/documentation/kmlreference.html}
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link{kmlLegend}}
}
\examples{
}
\keyword{graphics}
\keyword{aplot}


