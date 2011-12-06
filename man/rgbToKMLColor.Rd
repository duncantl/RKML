\name{rgbToKMLColor}
\alias{rgbToKMLColor}
\title{Convert an R color to KML color}
\description{
  This function attempts to be very flexible in taking
  a color specified for R and converting it to the format
  that KML uses to represent a color, namely ALPHA GREEN BLUE RED!

  This is exported so that others building on the RKML package
  can use it directly without requiring calls of the form
  \code{RKML:::rgbToKMLColor} (which are slow!).
}
\usage{
rgbToKMLColor(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the color to convert to KML form. This can be a named color
  such as "red" or "pink", or in RGB or RGBA format such as "#FF0000" or
  "#FF0000888", or a vector/matrix of one column of numbers giving the
  RGB components, i.e. returned from \code{col2rgb}.
  This is a single value, not a vector.}
}
\value{
 A character vector.
}

\note{
This is not vectorized at present.
}

\seealso{

}
\examples{
 rgbToKMLColor("red")
 rgbToKMLColor("#FF0000")
 rgbToKMLColor("#FF000099")
 rgbToKMLColor("#FF000099")

 rgbToKMLColor(col2rgb("red")) # Also starts a graphics device.
}
\keyword{graphics}

