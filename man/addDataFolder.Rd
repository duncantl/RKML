\name{addDataFolder}
\alias{addDataFolder}
\alias{addFolder}
\title{Create a new Folder element within the documnt}
\description{
  These two functions allow the caller to  add a new Folder 
  within a KML Document. A folder is used to store 
  grouped information and these are arranged and controlled hierarchically within the KML Document.
  That is we can make elements of a Folder visible or invisible collectively and
  this setting applies to all elements within the folder's hierarchy.

  \code{addFolder} creates a simple folder element, giving it a name and an optional id.

  \code{addDataFolder} creates a folder and populates it with Placemark sub-elements corresponding
  to the observations/rows in the specified data frame.
  The data frame is assumed to have variables named longitude and latitude
  points on the map are added at each of these for each time.
  And the elements are connected in time by a line.
  The appearances of the points and the lines are controlled by the elements of 
  \code{col} and \code{lty} which are the names of styles within the document.

  \code{addDataFolder} is used in the \code{kmlPanel.deault} function used, by default,
  in \code{kmlTime}.
}
\usage{
addDataFolder(data, times,
              folder = newXMLNode("Folder", if(length(id) > 0) attrs = c(id = id)), 
              col = "red", lty = "a", id = character(), addLines = FALSE,
              name = if(length(base:::names(times)))
                         base:::names(times)
                      else
                         times,
              description = name, dateFormat = "\%Y-\%m-\%dT\%H:\%M:\%S")
addFolder(name, id = character(), parent = NULL)
}
\arguments{
  \item{data}{the data frame giving the observations for each time point.
          This is assumed to have variables named longitude and latitude}
  \item{times}{a variable giving the times}
  \item{folder}{the XML Folder node, typically not specified}
  \item{col}{character vector giving the style for the points. These should be "anchored", i.e. start with \code{\#} to refer to the
      style within this Document, or be URLs.}
  \item{lty}{character vector giving the style for the lines joining the points}
  \item{id}{the value of the id attribute used to identify this node (not the name/label)}
  \item{name}{the label for this Folder which is displayed in the \code{Places} panel of the Google Earth GUI.}
  \item{parent}{the parent XML node for the new Folder node.}
  \item{addLines}{a logical value indicating whether to connect
  successive points with a line. This is useful when the points are
  related and represent a path.}
  \item{names}{the names to use to display for each point}
   \item{description}{a character vector with as many elements as there
    are points providing HTML content that is to be displayed when the
    viewer clicks on a point.}
  \item{dateFormat}{a string specifying the format for generating the
    date strings from POSIXt objects. This allows the caller to
    customize the generation of the date-time strings.}
}
\value{
 The newly created Folder XML node.
}
\references{
 \url{http://code.google.com/apis/kml/documentation/kmlreference.html\#folder}
}
\author{Duncan Temple Lang}
\seealso{
}
\examples{

}
\keyword{IO}
\keyword{hplot}
