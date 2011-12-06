library(RKML)
#
# The code for this example was initially created by Deb Nolan as an R script.
# This version has adapted that code into an R package to be more generally reusable
# and provide functions and design that illustrates how to build KML from within R using
# styles, etc.
#
# The example comes from David Brillinger based on work of he and his collaborator Stewart
# in studying the travels of elephant seals. The data we have here.
#
#

#
# A description of the styles.
# This is a list with named elements. The names are used as the names for the styles in the KML document.
# Each element is also a named list. It is intended to provide the equivalent hierarchy as in the 
# Style XML node. That is described in the KML reference manual as 
# <Style id="ID">
#<!-- extends StyleSelector -->
#<!-- specific to Style -->
#  <IconStyle>...</IconStyle>
#  <LabelStyle>...</LabelStyle>
#  <LineStyle>...</LineStyle>
#  <PolyStyle>...</PolyStyle>
#  <BalloonStyle>...</BalloonStyle>
#  <ListStyle>...</ListStyle>
#</Style>
#
# (See http://code.google.com/apis/kml/documentation/kmlreference.html#style)
#
# So in our list describing a style, the names identify the XML elements, e.g. IconStyle, LabelStyle, ...
# Within each top-level Stle node/element, there are sub-elements.
# We cannot distinguish between elements and attributes from this description alone.
# In the future, we will be able to map from an R object to the appropriate XML node by reading the XML
# schema for KML.
#
#
styles = list("ball_green" =
                 list("IconStyle" = list("scale" = "0.5",  "Icon" = c("href" = "http://www.stat.berkeley.edu/users/nolan/stat133/data/greendot.png")),
                     "BalloonStyle" = "$description"),
              "ball_red" =
                 list("IconStyle" = list("scale" = "0.5",  "Icon" = c("href" = "http://www.stat.berkeley.edu/users/nolan/stat133/data/reddot.png")),
                       "BalloonStyle" = "$description"),
              "line_green" = list(LineStyle = list(color = I("ff8adfb2"), width = 4)),
              "line_red" = list(LineStyle = list(color = I("ff999afb"), width = 2)))



 # Load the data from this package.
data(elephantSeal)

 # Generate the KML.
outDays = 44
direction = factor(c(rep("Out", outDays),
                     rep("Return", nrow(elephantSeal) - outDays)))

o = kmlTime(elephantSeal, times = elephantSeal$date, 
            name = format(elephantSeal$date, "%d-%m"),
            style = c("ball_green", "ball_red")[direction],
            lty = c("line_green", "line_red")[direction],
            docName = "Elephant Seal Travel Path",
            docDescription = "Elephant seal data <a href='http://www.stat.berkeley.edu/~brill'>Stewart & Brillinger</a>",
            docStyles = styles, addLines = TRUE, folderName = "Daily Tracks")

 saveXML(o, "elephantSeal.kml")


###################
library(RKML)
if(require(argosfilter)) {
data(seal, package = "argosfilter")
names(seal) = c("time", "latitude", "longitude", "lc")
o = kmlTime(seal, , "ball_green", "line_red",
            "Norwegian Harp Seal",
            "Seal location data. See argosfilter package and data(seal) <a href='http://cran.r-project.org'>CRAN</a>",
            styles = styles)
 saveXML(o, "harpSeal.kml")
}
