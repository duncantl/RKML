\name{createLookAt}
\alias{createLookAt}
\alias{addScreenOverlay}
\alias{addGroundOverlay}
\alias{addPlacemark}
\alias{addIcon}
\alias{addDescription}
\title{Create various KML nodes}
\description{
 These functions allow one to create various different types of KML nodes.
 The type of node created corresponds to the name of the function.
 The parameters typically correspond to sub-elements of these
}
\usage{
createLookAt(window, parent)
addGroundOverlay(doc, ..., parent = xmlRoot(doc)[["Document"]],
                  node = newXMLNode("GroundOverlay", parent = parent))
addScreenOverlay(doc, ..., parent = xmlRoot(doc)[["Document"]],
                  node = newXMLNode("ScreenOverlay", parent = parent))
addPlacemark(point, name, description = character(), id = character(), styleUrl = character(), time = NA, parent = NULL)
addIcon(file, color = character(), parent = NULL)  
addDescription(text, parent = NULL, cdata = TRUE, sep = "")
}
\arguments{
  \item{window}{a named vector giving the sub-elements of the LookAt node.}
  \item{parent}{the parent XML node.}
  \item{doc}{the top-level XML document whose Document node will act as the parent for the new node.}
  \item{...}{name = vale pairs that are used to populate the node.
    These correspond to sub-nodes or possibly attributes.}
  \item{node}{the node being created which can be specified by the caller in case she wants to use the function
   to fill in the node, but not use the standard name.}
  \item{styleUrl}{the name of a style or an explict URL. These should be anchored, i.e. prefixed with \# if local.}
  \item{point}{a vector of longitude and latitude values.}
  \item{name}{the name of the Placemar, used as a label in the folder in the "Places" view}
  \item{id}{the value of the id attribute to use for the new node.}
  \item{time}{the time for a TimeStamp element when creating the Placemark.}
  \item{file}{the name of the file for the Icon/image}
  \item{color}{the color for the image, given as a string.}
  \item{description,text}{the text for the description.}
  \item{cdata}{a logical value indicating whether the text for the description should be "escaped" within a CDATA block.}
  \item{sep}{the separator to use for the elements of the \code{text} vector
      when combining them to a single string. This is passed to \code{\link[base]{paste}}.}
}
\value{
 The newly created node.
}
\references{http://code.google.com/apis/kml/documentation/kmlreference.html}
\author{Duncan Temple Lang}

\seealso{
  There are more R-friendly functions for some of these low-level
  construction functions, e.g.
  \code{\link{groundOverlay}} and
  \code{\link{kmlLegend}}
}
\examples{

}
\keyword{IO}
\keyword{hplot}
\keyword{iplot}
