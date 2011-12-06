\name{kmlSegments}
\alias{kmlSegments}
\alias{kmlCircle}
\title{Draw circles and line segments on Google Earth in KML}
\description{
  These functions allow us to create display elements in KML
  for showing circles or collections of lines.

  These are works in progress and the details (e.g. styles and colors) will be filled in as we
  find time.
}
\usage{
kmlSegments(x0, y0, x1, y1, col = character(), lty = integer(),
            lwd = integer(), ..., doc = NULL, parent = NULL)
kmlCircle(x, y, r, parent = NULL, numLines = 60, col = NA,
           color = character(), scale = 1L, ..., styleId = NA)
}
\arguments{
  \item{x,y,x0,y0,x1,y1}{vectors of the coordinates for the end points of
    the line segments}
  \item{r}{the radius of the circle}
  \item{col}{currently ignored}
  \item{lty}{currently ignored}
  \item{lwd}{currently ignored}
  \item{\dots}{additional arguments that are currently ignored.}
  \item{doc}{currently ignored}
  \item{parent}{either \code{NULL} or an \code{XMLInternalNode}
    that is to be used as the parent XML node for the new KML content
    that is created.}
  \item{numLines}{an integer controlling how well the circle is
    rendered.  
     Circles are drawn in KML as a sequence of connected
    line segments that appear as a circle. The number of lines controls
   the resolution of the circle's approximation with a large number of
   lines improving the approximation and smoothness.}
  \item{scale}{the scale for the lines being used to render the circle,
    used as an attribute of the KML style.}
  \item{color}{a color specified as a KML string for the KML style}
  \item{styleId}{a string giving the name of the style to se for the circle.}
}
\value{
}
\references{
KML Reference Manual \url{http://code.google.com/apis/kml/documentation/kmlreference.html}
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link{kml}}
  \code{\link{createKMLDoc}}
}
\examples{
}
\keyword{IO}
\keyword{hplot}
\keyword{iplot}
