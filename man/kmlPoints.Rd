\name{kmlPoints}
\alias{kmlPoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Display locations in a KML document}
\description{
  This function displays the locations given in the data frame by longitude and latitude 
  via KML.
}
\usage{
kmlPoints(data = NULL, style = character(),
          docName = if(missing(data)) deparse(substitute(.longitude)) else deparse(substitute(data)),
          docDescription = NA,
          folderName = if(missing(data)) docName else deparse(substitute(data)),
          window = c(longitude = mean(.longitude, na.rm = TRUE),
                     latitude = mean(.latitude, na.rm = TRUE),
                     altitude = 4100000, tilt = 0, heading = 0),
          docStyles = I(""), panel = kmlPanel.points,
          type = "p", groups = character(),
          ..., panelVar = NULL,
          kml.doc = createKMLDoc(docName, docDescription, window, data),
          .longitude = data$long,
          .latitude = data$lat,
          ids = if(length(names(.longitude)))
                  names(.longitude)
                else
                  rownames(data),
          .names = if ("name" \%in\% names(data))
                      data$name
                   else
                      rownames(data),
          description = character(), col = character(), lty = character())
}
\arguments{
  \item{data}{the data frame containing variables longitude and latitude, at least,
         and also optionally description, name, id, styleUrl}
  \item{col}{a character vector giving the styleUrl names to use. This should have as many rows as \code{data}.}
  \item{docName}{a string giving the name of the document to appear in the Places panel in Google Earth.}
  \item{docDescription}{a string, potentially containing HTML, that describes the document}
  \item{folderName}{the name of the folder within this document for all the points. If this is NA, we don't create a folder but add the Placemark elements
     directly under the document. There are advantages and disadvantages to this.}
  \item{window}{a vector that is passed to \code{\link{createLookAt}} to specify the document's view point.}
  \item{docStyles}{a list of style definitions. These are added to the document and can be used by the Placemark nodes to control the appearance}
  \item{panel}{the function that actually creates the Placemark nodes for the observations in \code{data}.}
  \item{\dots}{additional arguments passed on to \code{panel}/}
  \item{kml.doc}{the KML document. This can be specified by the caller if it is already created.}
  \item{.longitude}{the numeric vector specifying the longitude values}
  \item{.latitude}{the numeric vector specifying the latitude values}
  \item{ids}{vector giving the identifiers/labels for each of the
    observations.}
  \item{.names}{the names to use for identifying the \code{Placemark}
    nodes. These are the strings displayed on the map. }
  \item{description}{a character vector giving a description string
    (typically HTML) for each \code{Placemark} elements being created,
    or an empty vector.}
  \item{type}{ a string controlling whether to draw points, lines, both, etc.
    See \code{\link[graphics]{plot.default}}.}
  \item{panelVar}{the variable used to group the observation into
    separate folders. This can be thought of as analogous to the
    conditioning variable in a lattice plot.}    
  \item{groups}{an empty vector or a vector treated as a  factor that is
    used to split the data into sub-groups within a panel/folder.}
  \item{style}{a vector of  style identifiers to use for the individual
    placemark elements.}
  \item{lty}{the line type which is converted to a style}
}

\value{
 A KMLDoc object which is an internal C-level XML tree.
}
\references{}
\author{Duncan Temple Lang}

\seealso{
 \code{\link{kml}}
 \code{\link{kmlTime}}
 \code{kmlPanel.points}
}
% http://en.wikipedia.org/wiki/List_of_United_States_cities_by_population
\examples{
 data(USCities, package = "RKML") 
 USCities$longitude = - USCities$longitude
 
 dd = kmlPoints(USCities, docName = "US Cities", folderName = NA, description = paste(USCities$name, USCities$state, sep = ", "))
 saveXML(dd, "cities.kml")

   # This shows using the formula notation to specify which variables are which.
 myData = USCities
 names(myData)[1:2] = c("Y", "X")
 dd = kml(~ X + Y, myData, "Cities")
 saveXML(dd, "myCities.kml")

   # use styles to display an additional attribute, e.g. average income, population, etc.
   #

  caCities = subset(USCities, name \%in\% c("Sacramento", "SanFrancisco", "LosAngeles"))[c(1, 2, 4),]

  doc = kmlPoints(caCities, addLines = TRUE)
  saveXML(doc, "lines.kml")
}
\keyword{IO}
\keyword{hplot}
