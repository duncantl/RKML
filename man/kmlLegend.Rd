\name{kmlLegend}
\alias{kmlLegend}
\title{Add a legend on a Google Earth screen}
\description{
  This function allows us to place a legend on a Google
  Earth display.   The legend can be used to identify
  different categories by color or  line type, for example,
  
  The legend can be a previously created image or alternatively
  we can use R to create a legend and display that.
}
\usage{
kmlLegend(x = 0, y = 0, legend, ..., parent = NULL, name = "legend",
           units = rep("pixels", 2), dims = c(100, length(legend) * 12 * 1.5),
           filename = "legend.png", bg = "transparent")
}
\arguments{
  \item{x,y}{the screen coordinates  starting at the bottom left.}
  \item{legend}{the name of the file containing the legend or
     a vector of strings giving the labels for the legend.}
   \item{\dots}{additional arguments passed on the \code{\link{legend}}
    if we are creating the legend within R.}
  \item{parent}{an XML internal node that is used as the parent for the
  new ScreenOverlay node being created. This can also be the \code{KMLDoc}
  object representing the entire KML document. In this case, the parent
  is taken as the \code{Document} node directly under the root node of the document.}
  \item{name}{a string that is displayed as the name for the
  ScreenOverlay node in the KML hierarchy.}
  \item{units}{a character vector giving the units for the x and y
    coordinates.
    The units can be "pixels", "fraction", "insetPixels".}
  \item{dims}{an integer vector of length 2 that is used if we create
     the legend in R. These give the dimensions to use when creating  a PNG
     file for the legend giving the width and height in pixels.}
  \item{filename}{the name of the file to create if we are creating the
  legend image in R.}
  \item{bg}{the background to use for the call to \code{\link{png}}
   if we are creating the legend image in R.}
}
\value{
 An XMLInternalNode whose name is GroundOverlay.
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

