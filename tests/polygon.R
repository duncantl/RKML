library(RKML)
library(RKML)
data(USCities)

USCities$longitude = - USCities$longitude

cities = c("Sacramento", "SanFrancisco", "Oakland", "SantaBarb", "LosAngeles", "SanDiego", "Stockton")
x = subset(USCities, state == "CA" & name %in% cities)
x = x[!duplicated(x$name),]
rownames(x) = x$name
x[cities,]


doc = createKMLDoc("Example of Polygons",
                   "Locations of a few cities in california connected as a polygon" ,
                    window = c(longitude = median(x$long),
                               latitude = median(x$lat)))

kmlPolygon(x$long, x$lat, parent = xmlRoot(doc)[["Document"]],
           fill = 1, col = "#00FF0044"
           # style = list(tmp = list(PolyStyle = list(color = "#FF0000", width = 3)))
           )
saveXML(doc, "polygon.kml")

if(!file.exists("upPoly.kml") && exists("Open", mode = "function")) {
  createUpdatingDoc("polygon.kml", out = "upPoly.kml")
  Open("upPoly.kml")
}


