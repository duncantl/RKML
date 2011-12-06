\name{createUpdatingDoc}
\alias{createUpdatingDoc}
\title{Create KML document that reloads another document at regular intervals}
\description{
 This function creates a simple KML document that references another KML
 document and arranges to reload that second file at regular intervals.
 This can be used when the contents of the second file  are changing
 regularly. This might be because the data are changing, or becuase
 we are experimenting with the KML file and want to see it updated.
 This is convenient for debugging the creation of KML fliles, but
 is also 
}
\usage{
createUpdatingDoc(targetFile, interval = 5, window = c(),
                   docName = targetFile,
                    description = sprintf("Updating of \%s file", targetFile),
                     out = NA, ..., doc = createKMLDoc(targetFile, description, window, ...))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{targetFile}{the name of the file to reload at regular intervals}
  \item{interval}{the number of seconds between reloading the target file}
  \item{window}{the coordinates to view when the file is loaded}
  \item{docName}{a string to display in Google Earth's "Places" list
    that identifies this document (not the one it loads)}
  \item{description}{a string giving a more detail description of this document.}
  \item{out}{if not empty or \code{NA} this is the name of a file to
    which to write the new KML file that updates the display of
    \code{targetFile}.
     In other words, this writes the updating file and this is the name
     of that new updating file.
   }
   \item{\dots}{additional arguments passed on to create the XML/KML
     document}
   \item{doc}{the KML document object that is created and returned.
     The caller can provide this to control its creation but allow the
     function to fill in the information within the document.}
}
\details{
This creates a KML document with a NetworkLink  element.
}
\value{
 An object of class KMLDoc which is an XML document.
}
\references{
\url{http://code.google.com/apis/kml/documentation/kmlreference.html#networklink}
}
\author{Duncan Temple Lang}

\seealso{}
\examples{
library(RKML)
dyn = createUpdatingDoc("foo.kml")
saveXML(dyn, "dyn.kml")

if(Sys.info()["sysname"] == "Darwin")
   system("open dyn.kml")

data(USCities)
foo = kml( ~ longitude + latitude, USCities[1:50,])
saveXML(foo, "foo.kml")
}
\keyword{plot}
