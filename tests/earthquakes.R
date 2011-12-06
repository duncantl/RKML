library(RKML)
                  #eq = read.csv("eqMag.csv", header=TRUE)
data(quakes, package="RKML")

        # Set up a list of styles to use, one for each order of magnitude
        # They all look the same, but are scaled according to magnitude
        # This is the generic style, we will change the value of scale 
        # to create the others.
        # We use the yellow dot from USGS site (downloaded to local file)

ballStyle =  list("IconStyle" = list("scale" = "1",
            "Icon" = c("href" = "yellowdot.png")))
ballNames = paste("ball_", 0:8, sep="")

        # These scales match those that the USGS use 
magScales =  as.character((1:10/10)[-7])

styles = lapply(1:9, function(i) {
    ballStyle$IconStyle$scale = magScales[i]
    ballStyle
})
names(styles) = ballNames

Bsize = floor(quakes$magnitude)
Bsize[Bsize>8] = 8
quakeStyle = names(styles)[Bsize + 1]


         # the kmlPoints function creates the kmz file.
         # Note that descriptions contains the description for each point

doc = kmlPoints(quakes, "Earthquakes", 
      docDescription = "Earthquakes over a 7 day period. These data were extracted from the USGS website: http://earthquake.usgs.gov/earthquakes/catalogs/merged_catalog.xml.gz",  # add a description to the document
      col = quakeStyle, #add style to each of the points
      folderName="Quake Locations", # name of the folder to contain points
      .names = quakes$magnitude,  # name will appear next to the placemark
      description =  
 sprintf("<table><tr><td>Date:</td><td>%s-%s-%s</td></tr><tr><td>Magnitude:</td><td>%s</td></tr><tr><td>Depth:</td><td>%s</td></tr></table>", quakes$year, quakes$month, quakes$day, quakes$magnitude, quakes$depth), 
     #individual descriptions appear in pop-up window for each placemark
      styles = styles
)

saveXML(doc, "eqPlainX.kml")


# Ignore this old stuff
describeRow = function(x) {
  namesX = names(x)
  kids = sapply(1:length(x), function(i) 
         newXMLNode("Data", newXMLNode("value", x[i]), attrs=c(name=namesX[i])))
  saveXML(newXMLNode("ExtendedData", .children = kids))
}

