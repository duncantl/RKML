library(RKML)
data(USCities, package = "RKML")
USCities$longitude = - USCities$longitude

dd = kmlPoints(USCities[1:100,], docName = "US Cities", folderName = "cities", description = paste(USCities$name[1:100], USCities$state[1:100], sep = ", "))
kmlLegend(legend = I(system.file("sampleKML", "legend.png", package = "RKML")),
                     parent = xmlRoot(dd)[["Document"]])
#makeKMZArchive(dd, "/tmp/legend.kmz")
saveXML(dd, "legend.kml")


if (FALSE) {
#Deb temporary
dd = kmlPoints(USCities[1:100,], docName = "US Cities", 
         folderName = "cities",
         description = paste(USCities$name[1:100], USCities$state[1:100], sep = ", "))
kmlLegend(legend = c(LETTERS[1:10]), lty = 1:10, col = rainbow(10), lwd = 1:10,
              parent = xmlRoot(dd)[["Document"]])
saveXML(dd, "legend1.kml")

saveXML(createUpdatingDoc("legend1.kml"), "bar.kml")
if(exists("Open", mode = "function"))
 Open("bar.kml")
}
  #makeKMZArchive(dd, "/tmp/legend.kmz")

