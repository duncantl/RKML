\name{makeBallIcon}
\alias{makeBallIcon}
\alias{makeRectIcon}
\alias{makePchIcon}
\title{Create an icon image file}
\description{
These functions are used to create icon images for placing 
on Google Earth (i.e. an icon for a place mark on the earth).   
}
\usage{
makeBallIcon(col = "red", file = "/tmp/ball.png", width = 40, height = width, radius = 0.43)
makeRectIcon(col = "red", file = "/tmp/rect.png", width = 40, height = width, radius = 1)
makePchIcon(pch = "red", col = "red", file = sprintf("pch_\%s.png", as.character(pch)),
              cex = 4, width = 40, height = width, radius = 1)
}
\arguments{
  \item{col}{a color used to control the color of the shape or character
    being drawn, passed to a call to \code{symbols}, \code{points} or \code{text}.}
  \item{file}{the name of the PNG file to create}
  \item{width,height}{the width and height of the image to create, in pixels}
  \item{radius}{the size of the symbol to draw}
  \item{pch}{the plotting character to draw in the image}
  \item{cex}{character expansion factor passed on to \code{\link[graphics]{text}}.}
}
\value{
 a character string giving the name of the generated PNG file.
}

\author{
Duncan Temple Lang
}

\seealso{
 \code{\link{kml}}, \code{\link{kmlPoints}} and the \code{styles} parameter.
}
\examples{
  makeBallIcon("red")
  makePchIcon(17, "#0000FF")

  mapply(makeRectIcon, heat.colors(7), sprintf("heat_\%d.png", 1:7))

  data(quakes)

  mag = factor(as.integer(quakes$magnitude))
  icons = mapply(makeBallIcon, heat.colors(length(levels(mag))), sprintf("heat_\%d.png", 1:length(levels(mag))))
  styles = lapply(icons, function(x) list(IconStyle = list(scale = ".5", Icon = c(href = x))))
  names(styles) = gsub(".png", "", icons)

  doc = kml(~ longitude + latitude, quakes, styles = styles,  col = names(styles)[mag])
}
\keyword{graphics}
\keyword{programming}

