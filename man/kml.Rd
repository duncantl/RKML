\name{kml}
\alias{kml}
\title{High-level KML generation}
\description{
 This function is a high-level interface to create KML document from R data.
 The idea is that it will be extended to call different KML generation
 functions. For the moment, it calls kmlTime or kmlPoints.

 This is a generic function with methods for various types of inputs.
 The important one is the formula interface. We can specify  the 
 names of the variables in our data frame that identify
 longitude and latitude, time.
 The formula language we use is similar to that of R's modelling
 and lattice graphics systems. But we introduce the notion of time
 which is specified via the \code{@} symbol, e.g.
 \code{ ~ X + Y @ Time}.

 The intent is to allow transformations of these variables, including
 allowing the invocation of functions on the right-hand-side of the
 formula, e.g. \code{view(X, Y, Time) ~ X + Y @ Time},
 \code{hist(X, Y) ~ X + Y}.
 The function can create KML nodes or returns objects of a class for which there is
 a method for the toKML function.
 
 More thought is needed to make this coherent and useful.
}
\usage{
kml(x, data, folderName = deparse(substitute(data)),
     docDescription = deparse(substitute(x)),  ...)
}
\arguments{
  \item{x}{a formula or data frame}
  \item{data}{the data frame}
  \item{folderName}{a character string giving the name to use for the
      folder in the Google Earth display for this collection of display elements}
  \item{docDescription}{a character string giving a description for this
    display. This is displayed when the Google Earth user double clicks
    on the entry in the Places list.
   }

  \item{\dots}{additional arguments for the methods}
}
\value{
 Typically, a KMLDoc object which is an XML (internal) document.
}
\references{}
\author{Duncan Temple Lang}

\seealso{ 
\code{\link{kmlTime}}
}
\examples{

}
\keyword{IO}
\keyword{hplot}
\keyword{iplot}
