\name{makeKMZArchive}
\alias{makeKMZArchive}
\alias{makeKMZ}
\alias{saveXML,KMLDoc,character-method}
\title{Save the in-memory KML document to a KMZ file}
\description{
  This function is used to serialize a KML document to a file.
  If it is large or has references to local files, we create a KMZ file
  to create a compressed archive containing all the files. This can then
  be distributed as a single entity.
  Alternatively, the single document is serialized to a regular KML
  file.
}
\usage{
makeKMZArchive(doc, file = docName(doc), ...,
               force = TRUE, clone = TRUE, useKMZ = NA,
                baseDir = getwd(), xpathQuery = "//Icon/href|//Icon/text()|//image[@src]",
                fixHTMLRefs = TRUE)
}
\arguments{
  \item{doc}{the in-memory KML document that is to be serialized}
  \item{file}{the name of the output file. If this is an object of class
    \code{AsIs}, we write directly to that file and do not attempt to
    determine if we should create a KMZ file.}
%  \item{basedir}{the directory relative to which relative file
%    references in the \code{doc} are resolved}
  \item{\dots}{additional parameters currently passed to \code{\link[XML]{saveXML}}.}
%  \item{zipCmd}{the command to be used for invoking the zip executable.}
  \item{force}{a logical value that controls whether to force the search
  for images and create a KMZ file.}
  \item{clone}{a logical value controlling whether we should copy (or
    clone) the XML document before making any changes to it as we create
    a KMZ archive.}
  \item{useKMZ}{a logical value that controls whether to use KMZ or just
    KML. If this is left as \code{NA}, we determine which format to use
     based on whether there are external local files or if the KML
     document is more than 10 KiloBytes.
   }
   \item{baseDir}{a character vector. This is used when we rename local
     files as entries in the ZIP archive we create.  The idea is
     possibly simplest to explain with an example.
     Suppose our KML document refers to an Icon file in the RKML package
     using the full path,  e.g. /Users/duncan/Rpackages/RKML/Icons/reddot.png.
     We don't want this full path in the KMZ archive, but probably want
     Icons/reddot.png. So we want to remove the prefix
     /Users/duncan/Rpackages/RKML/.
     This parameter \code{baseDir} allows us to do this.
     The contents are the prefixes we want removed from the names of any
     local files referenced in the KML document. We can specify one or
     more such prefixes.  The trailing / is append to the directories
     for you, but one can specify the values with a /.}
   \item{xpathQuery}{the xpath query string used to find the nodes of
     interest that may have references to external files.}
    \item{fixHTMLRefs}{a logical value that controls whether we check
      for references to local external files within the HTML code within
      \code{description} nodes.  This can be quite time consuming.
     So this parameter allows the caller to avoid doing this search if
     she knows there are no external references or if she will handle
     them by herself, e.g. add them to the KMZ file after it is created.}
}
\value{
The name of the file that was created, a character vector of length 1.
}
%\references{}
\author{
Duncan Temple Lang
}
\seealso{
\code{\link[XML]{saveXML}}
}

\examples{
styles = list("ball_green" =
                 list("IconStyle" = list("scale" = "0.5",
                                         "Icon" = c("href" = system.file("Icons", "greendot.png", package = "RKML"))),
                                         "BalloonStyle" = "$description"),
              "ball_red" =
                 list("IconStyle" = list("scale" = "0.5",
                                         "Icon" = c("href" = system.file("Icons", "reddot.png", package = "RKML"))),
                                         "BalloonStyle" = "$description"),
              "line_green" = list(LineStyle = list(color = I("ff8adfb2"), width = 4)),
              "line_red" = list(LineStyle = list(color = I("ff999afb"), width = 2)))

data(elephantSeal)
outDays = 44
direction = factor(c(rep("Out", outDays),
                     rep("Return", nrow(elephantSeal) - outDays)))

o = kmlTime(elephantSeal, elephantSeal$date, 
            name = format(elephantSeal$date, "\%d-\%m"),
            style = c("ball_green", "ball_red")[direction],
            lty = c("line_green", "line_red")[direction],
            "Elephant Seal Travel Path",
            "Elephant seal data <a href='http://www.stat.berkeley.edu/~brill'>Stewart & Brillinger</a>",
            docStyles = styles)


makeKMZArchive(o, "seal.kmz")
}
\keyword{IO}
\keyword{programming}
