\name{HTMLTable}
\alias{HTMLTable}
\alias{HTMLTable,vector-method}
\alias{HTMLTable,data.frame-method}

\title{Create a simple HTML table to represent an R vector}
\description{This is a very simple utility function
  to create an HTML table that displays information from
  an R object, typically just a simple one-dimensional vector
  with names. It is used to create the description for a point in
  a KML display. When the viewer clicks on a point, this is displayed
  in a "balloon".

  This is not intended to be as intelligent and flexible as
  functions in the \code{XML} or XDynDocs or RWordXML or R2HTML packages
  for representing arbitrary R objects in HTML or another format.
}
\usage{
HTMLTable(x, labels = names(x), byRow = TRUE, ...)
}
\arguments{
  \item{x}{the R object which is to be displayed as a table, currently a
  named vector}
  \item{labels}{column names for the table}
  \item{byRow}{currently ignored but potentially a logical value that
  would control whether to organize the display of the elements by colum or row}
  \item{\dots}{additional parameters for methods}
}
\value{
  An XMLInternalNode representing the table.
  To convert this to a character representation of the table,
  use \code{\link[XML]{saveXML}}.
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link{kmlTime}} and   \code{\link{kmlPoints}}
   or the general   \code{\link{kml}}
}
\examples{
  HTMLTable(1:3)
  HTMLTable(1:3, byRow = FALSE)
  HTMLTable(structure(1:3, names = c("ABC", "DEF", "GHI")))
  HTMLTable(structure(1:3, names = c("ABC", "DEF", "GHI")), byRow = FALSE)
  HTMLTable(mtcars)
}
\keyword{IO}
\keyword{hplot}
\keyword{iplot}
