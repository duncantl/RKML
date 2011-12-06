\name{kmlTime}
\alias{kmlTime}
\title{Create a KML animation}
\description{
 This is the lower-level version that one can call directly 
 or indirectly via the generic \code{\link{kml}}
 formula which understands an extended formula language.
}
\usage{
kmlTime(data, times = data$time, style = character(), lty = character(), 
        docName = deparse(substitute(data)), docDescription = NA,
        folderName = deparse(substitute(data)),
        window = c(longitude = mean(.longitude, na.rm = TRUE),
                   latitude = mean(.latitude, na.rm = TRUE),
                    altitude = 4100000, tilt = 0, heading = 0),
        docStyles = list(), groups = character(), panel = kmlPanel.default, ...,
         kml.doc = createKMLDoc(docName, docDescription, window, data),
        .longitude = data$long, .latitude = data$lat,
        ids = if (length(names(.longitude)))
                names(.longitude)
              else
                rownames(data))
}

\arguments{
  \item{data}{a data frame}
  \item{times}{the time variable as a vector. If this is has a names attribute, these names are used to label the points.}
  \item{style}{a vector of style names controlling the appearance (color, image, etc.) of the points on the KML display.
              This should have as many elements as there are time points in the 
             data set}
  \item{lty}{a vector of style names controlling the appearance of the lines connecting the points on the KML display.
              This should have as many elements as there are time points in the 
             data set}
  \item{docName}{the name of the document (not the file) used within the KML meta-data. If this is not specified, 
                    this is taken from the "name" of the data, i.e. the expression for the first argument.}
  \item{docDescription}{a character string giving a human-readable description of the KML document. This is added to the 
    KML meta-data and displayed to the viewer in Google Earth.}
  \item{folderName}{the name of the folder within the KML document }
  \item{window}{a named vector specifying values for the KML LookAt element. This includes 
        latitude and longitude, altitude, heading, tilt, range and altitudeMode.
        latitude and longitude are computed from the mean of the corresponding elements in the 
        data frame.  (Currently, the names of the variables representing these variables are hard coded.) }
  \item{docStyles}{a list KML style nodes (as XML internal nodes) or a list of
                 objects which define the contents of a KML style. See \code{\link{makeStyle}}.}
%  \item{byRef}{currently ignored!  This used to be to control whether the styles were used directly inline or by reference.
%       The latter is greatly preferred when the same styles are reused many times.}
  \item{groups}{a vector that is used to group the observations in the dataset into sub-groups,
       much like within a panel in lattice, so that the sub-group can be
       displayed as a unit.
       The purpose of this is to group the graphical elements displayed
       in KML in separate folders, one for each level of the group factor.
     }
  \item{panel}{the function that is used to generate the KML content for each  of the sub-groups
        or the entire data as a single unit if there are no groups
	specified.}
  \item{\dots}{additional arguments that are passed directly to the
    \code{panel} function in each call.}
  \item{kml.doc}{the KML document. This can be specified by the caller if it is already created.}  
  \item{.longitude}{the numeric vector specifying the longitude values}
  \item{.latitude}{the numeric vector specifying the latitude values}
  \item{ids}{vector giving the identifiers/labels for each of the observations.}  
}
\details{

}
\value{
 An XML/KML document, returned invisibly so it does not print on the console.
}
\references{}
\author{Duncan Temple Lang}
\seealso{
\code{\link{kml}}
}
\examples{
styles = list("ball_green" =
                 list("IconStyle" = list("scale" = "0.5",
                                         "Icon" = c("href" = system.file("Icons", "greendot.png", package = "RKML"))),
                                         "BalloonStyle" = "$[description]"),
              "ball_red" =
                 list("IconStyle" = list("scale" = "0.5",
                                         "Icon" = c("href" = system.file("Icons", "reddot.png", package = "RKML"))),
                                         "BalloonStyle" = "$[description]"),
              "line_green" = list(LineStyle = list(color = I("ff8adfb2"), width = 4)),
              "line_red" = list(LineStyle = list(color = I("ff999afb"), width = 2)))

data(elephantSeal)
outDays = 44
direction = factor(c(rep("Out", outDays),
                     rep("Return", nrow(elephantSeal) - outDays)))
   
docDescription =  "Elephant seal data <a href='http://www.stat.berkeley.edu/~brill'>Stewart & Brillinger</a>"

o = kmlTime(elephantSeal, times = elephantSeal$date, 
            name = format(elephantSeal$date, "\%d-\%m"),
            docName = "Elephant Seal Travel Path",
            docDescription = docDescription,
            style = c("ball_green", "ball_red")[direction], 
            lty = c("line_green", "line_red"), 
            docStyles = styles, addLines = TRUE)

saveXML(o, "elephantSeal.kml")


o = kmlTime(elephantSeal, time = elephantSeal$date, 
            name = format(elephantSeal$date, "\%d-\%m"),
            style = c("ball_green", "ball_red")[direction], 
            lty = c("line_green", "line_red")[direction],
            docName = "Elephant Seal Travel Path",
            docDescription = docDescription,
            groups = gl(2, nrow(elephantSeal)),
            docStyles = styles, addLines = TRUE)

}
\keyword{IO}
\keyword{hplot}
\keyword{iplot}
\concept{KML}
\concept{Google Earth}
