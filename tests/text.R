library(RKML)
data(USCities, package = "RKML")
USCities$longitude = - USCities$longitude

doc = createKMLDoc("Example of text()",
                   'This is an example of using the text() method in RKML',
                   window = c(latitude = 38.5, longitude = -96))

text(doc, USCities$long[1:100], USCities$lat[1:100],  USCities$name[1:100])
saveXML(doc, "text.kml")

if(exists("Open", mode = "function"))
  Open("text.kmz")

