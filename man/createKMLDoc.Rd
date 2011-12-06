\name{createKMLDoc}
\alias{createKMLDoc}
\title{Create the top-level KML Document}
\description{
 This creates the XML document to represent a KML document.
 Google Earth can display multiple KML document objects
 simultaneously.

 In addition to creating the top-level \code{<kml>} node,
 it also creates the \code{<Document>} and populates it with
 meta data.  This gives the document a name that appears in the
 "Places" panel of the Google Earth display.  The description
  is the HTML text that is displayed if the viewer
  clicks on the Folder entry for this document in the "Places" view.

  We can also specify the initial view or "window" for this document
  which Google Earth will move to if the viewer selects this document
  by double-clicking on it.
}
\usage{
createKMLDoc(docName, description, window = c(longitude = 0, latitude = 0),
               data = NULL, ns = character())
}
\arguments{
  \item{docName}{the name to use in the Places panel}
  \item{description}{a description of this document. This can contain HTML markup.
      It can be the name of a file in which case its contents are used as the text for the description.
      To avoid this, use \code{I("the actual text")}. Alternatively, this can be a connection and the
       contents will be read.  Also, one can pass an XML node.}
  \item{window}{a vector containing named values for specifying the initial "view" for this document.
      This is passed to \code{\link{createLookAt}}. The important elements are 
      \code{longitude} and \code{latitude}.}
  \item{data}{an optional data frame assumed to have variables named longitude and latitude }
  \item{ns}{a character vector giving the names pace for the top-level
    node.  This defaults to kml as the prefix and
    \code{"http://earth.google.com/kml/2.2"} as the URI. One can remove
    the namespace by specifying an empty character vector.  Google Earth
    works with either but the presence of the namespace is "more correct".}
}
\value{
 An object of class KMLDoc which is the top-level XML internal document.
}
\references{\url{http://code.google.com/apis/kml/documentation/kmlreference.html}}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{kmlTime}}
 \code{\link{kml}}
}
\examples{
}
\keyword{IO}
\keyword{hplot}
\keyword{iplot}
