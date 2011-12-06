\name{addTimeStamp}
\alias{addTimeStamp}
\alias{addTimeStamp.POSIXct}
\alias{addTimeStamp.POSIXlt}
\alias{addTimeStamp.default}
\alias{addTimeStamp.Date}
\title{Adds a TimeStamp element to a node}
\description{
 This creates a TimeStamp XML element and adds it
 to the parent, if specified.
}
\usage{
addTimeStamp(when, parent = NULL, format = "\%Y-\%m-\%dT\%H:\%M:\%S\%Z")
}
\arguments{
  \item{when}{the time. This can be a POSIXct or POSIXlt object, a Date or a string that is already in the 
       appropriate format - the dateTime format specified in the general XML schema.}
  \item{parent}{an XML internal node to which to add the new TimeStamp
    node}
  \item{format}{the format string used by \code{\link[base]{strptime}}.}
}
\value{
 the XMLInternalNode object
}
\references{http://code.google.com/apis/kml/documentation/kmlreference.html}
\author{Duncan Temple Lang}
\seealso{ }
\examples{
}
\keyword{IO}

