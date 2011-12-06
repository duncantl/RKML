\name{folderNames}
\alias{folderNames}
\alias{folderNames,KMLDoc-method}
\alias{folderNames,AsIs-method}
\alias{folderNames,character-method}
\alias{folderNames,XMLInternalDocument-method}
\title{Query the names of the folders in a KML document}
\description{
  This function and its methods retrieve the value of the
  name element of each top-level Folder element in the document.
}
\usage{
folderNames(x, ...)
}
\arguments{
  \item{x}{the KML document, either as a file name or URL or a
    previously parsed XML document}
  \item{\dots}{additional arguments for methods}
}
\value{
 A character vector.
}
\references{
  KML Reference Manual
}
\author{
Duncan Temple Lang
}
\note{
  This could be made to be recursive and compute the names of sub-folders.
}


\seealso{
  \code{\link{addFolder}}
}

\examples{
 f = system.file("sampleKML", "images.kml", package = "RKML")

 folderNames(f)

 doc = xmlParse(f)

 folderNames(doc)
}
\keyword{IO}
\keyword{programming}
