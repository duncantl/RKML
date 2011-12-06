\name{setAPIKey}
\alias{setAPIKey}
\alias{copyPluginJSCode}
\title{Copy and Customize HTML and JavaScript Code for GE Plugin
  Instance in a Web page}
\description{
  These functions provide access to the HTML and JavaScript code
  that provide templates for creating a Web page with a Google Earth (GE)
  plugin instance  embedded within it.
  The templates are provided as part of the RKML package
  and need to be customized in different ways to present
  the plugin and text and controls the author desires.
  While most of the content is context-specific, the templates
  provide the basic framework which can be easily customized
  and facilitate the initial work which can be hard to remember.

  \code{setAPIKey} inserts the authors own Google Earth API key
  into the HTML document so that the basic GE code can be loaded.
  It uses the HTML document we provide in the package by default, but
  an alternative can be provided by the author. It will replace any
  existing key, so can be used to customize a page found on the Web, for
  example.

  \code{copyPluginJSCode} retrieves the template JavaScript code for the
  plugin and either writes it to a file or returns it as text.
  This function can also modify the code to load a KML document into the
  GE plugin instance.
}
\usage{
setAPIKey(key = getOption("GoogleEarthAPIKey",  Sys.getenv("GOOGLE_EARTH_API_KEY")), 
           doc = system.file("pluginTemplate", "gePluginTemplate.html", package = "RKML"))

copyPluginJSCode(to = "geInitialization.js",  kml = character(),
                 multipleGEs = FALSE,
                 source = system.file("pluginTemplate",
                                      if(multipleGEs)
                                        "geMultiInitialization.js"
                                      else
                                        "geInitialization.js",
                                      package = "RKML"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{key}{a string giving the Google Earth API key for the particular
    host on which the document will be made available. Each key is
    specific to a particular domain.
    You can obtain a key for free at
    \url{http://code.google.com/apis/maps/signup.html}.
    The caller can specify this explicitly. However, it is often more
    convenient
    to retrieve it from R's global options, using the option name
    \code{GOOGLE_EARTH_API_KEY}. We would set this in our R startup
    script,
    i.e. the \code{.Rprofile} file.
   }
  \item{doc}{a string giving the name of the HTML file to use as the
    template HTML for the Web page.  If this does not alreay have a
    script element in the head of the document to load the GE code,
    \code{setAPIKey} adds it. Otherwise, the function inserts the key.}

  \item{to}{the name of the file to write the code to.  This can be
    \code{NA} in which case the code is returned as a character vector
     and the  caller  can process it subsequently and perhaps insert it
    directly into an HTML document.
   %See \code{\link[RJSONIO]{addJavaScript}} in the \code{RJSONIO} package.
  }
  \item{kml}{a character vector giving the names of one or more KML
  files or URLs that are to be loaded into the Google Earth plugin
  instance(s). These are added to the initialization callback that is
  invoked by JavaScript when the GE instance has been created.
  Currently, we load them all into the same GE if \code{multipleGEs} is
  \code{FALSE}. If it is \code{TRUE}, the \code{kml} vector
  should have names  that correspond to the id values of the
  HTML elements into which the KML documents are to be loaded.
  }
  \item{source}{a string giving the name of the file containing the
    JavaScript code.}  
  \item{multipleGEs}{a logical value that is only used if \code{kml}
      contains more than one file name or URL. If this is
    \code{FALSE}, each of the KML documents is loaded into the same GE instance.}
}
\value{
\code{setAPIKey} returns a parsed XML docment.
You can process this further and then write it to a file
using \code{\link[XML]{saveXML}}.

\code{copyPluginJSCode} either writes the JavaScript
code to a file (specified by \code{to}) or
returns the code as a character vector.
}
\references{

}
\author{
Duncan Temple Lang
}

\seealso{}

\examples{
  doc = setAPIKey("ABCDEF")
  saveXML(doc, tempfile())

  txt = copyPluginJSCode(NA, "http://www.omegahat.org/RKML/JSM2011/tas1970.kml")

  txt = copyPluginJSCode(tempfile(),  "http://www.omegahat.org/RKML/JSM2011/tas1970.kml")  

  base = "http://www.omegahat.org/RKML/JSM2011/imageAnimation"
  docs = c(ge.timeslice = "timeslice_current.kml",
           ge.RCM = "gcmrcm_current.kml")
  txt = copyPluginJSCode(NA, structure(sprintf("\%s/\%s", base, docs),
      	           	         	names = names(docs)))

}
\keyword{visualization}
\keyword{programming}
\keyword{IO}
