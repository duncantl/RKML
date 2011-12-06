\name{makeStyle}
\alias{makeStyle}
\alias{addStyles}
\alias{anchorStyles}

\title{Create and add styles to a KML document}
\description{
  These functions allow one to create and add definitions
  of styles to control the appearance of KML objects.
  The idea is that we define a library of styles "centrally" and refer
  to these when creating KML objects that appear on the globe.
}
\usage{
makeStyle(attrs, id, parent = NULL)
addStyles(doc, docStyles)
}
%, col = character(), lty = character()) 
\arguments{
  \item{attrs}{a list or vector giving the elements of the style.}
  \item{id}{a string used as the identifier/name for the style}
  \item{parent}{the XML node to which this new style should be added as a child node}
  \item{doc}{the parsed KML document to which to add the style definitions}
  \item{docStyles}{a list of style definitions/descriptions}
%  \item{col}{}
%  \item{lty}{}
}

\value{
 The newly created XML Style node.
}
\references{http://code.google.com/apis/kml/documentation/kmlreference.html\#style}
\author{Duncan Temple Lang}
\seealso{}
\examples{

}
\keyword{IO}
\keyword{hplot}

