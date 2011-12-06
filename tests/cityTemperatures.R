library(RKML)

 # Load the data.
if(!exists("temperature") || class(temperature) != "data.frame") {
   # Note that this version has intentional errors.
  data(temperature, package = "RKML") # url("http://eeyore.ucdavis.edu/stat32/Data/Temperature.rda"))
  temperature$level = cut(temperature$temp, 4)

  if(!all(temperature$long < 0))
      temperature$longitude = - temperature$longitude
}
  # Generate some colors to use to represent the different levels of temperature
temperatureColors = rev(rainbow(length(levels(temperature$level))))


doc = createKMLDoc("City Temperatures",
                   'Illustrates the temperatures of 50 different <a href="cityInfo.html">cities</a> at 4 different times of the year.\nGenerated from R by <a href="http://www.stat.ucdavis.edu/~duncan">Duncan Temple Lang</a>', window = c(latitude = 38.5, longitude = -96))

  # Add a style that we will use in our Placemark
style = list(IconStyle = list(scale = .25, icon = "greendot.png"),
             LabelStyle = list(scale = 1,  color = "#FFFFFF00"))
addStyles(xmlRoot(doc)[["Document"]], list(me = style))


imageName =
  # This determines the name of the image containing the 4 point time series for the given city.
function(city, full = FALSE, mini = FALSE, ext = "png", dir = "cityTemperatures") {
  id = gsub("[ ,]", "_", city)

  if(is.logical(full)) {
     if(full)
       id = paste("file://", dir, id, sep = .Platform$file.sep)
     else
       id = paste(dir, id, sep = .Platform$file.sep)       
  } else
       id = paste(full, id, sep = .Platform$file.sep)      

  if(mini)
    id = paste(id, "mini", sep = "_")

  id = paste(id, ext, sep = ".")
  id
}

f =
  #  loop over the cities and process each of the specified
  #  months
function(docNode, showImages = FALSE, style = "me", months = levels(temperature$month))
{
 invisible(
  by(temperature, temperature$city,
     function(city) {

       cityName = strsplit(city[1, "city"], ",")[[1]][1]
       folder = addFolder(cityName, parent = docNode)

       by(city, city$month,
           function(row) {

                 mnth = as.character(row[1, "month"])
                 label = if(length(months) > 1) paste(row[1, "temp"], mnth) else row[1, "temp"]
                 mark = addPlacemark(c( as.numeric(row["longitude"]),  row["latitude"]), name = label, # id = row[1, "city"],
                                     parent = folder)
             

                 if(showImages) 
                   addIcon(imageName(city[1, "city"]), parent = mark)
                 else 
                   newXMLNode("styleUrl", paste("#", style, sep = ""), parent = mark)                   

                 addTimeStamp(when = paste("1997", sprintf("%02d", match(mnth, month.name)), sep = "-"), parent = mark)

                 
                 if(length(months) > 1) {
                   desc = list(cityName,
                               "<table>",
                               c("<tr>", paste("<td>", months, "</td>", sep = ""), "</tr>"),
                               c("<tr>", paste("<th>", city$temp, "</th>", sep = ""), "</tr>"),
                               "</table>",
                               c("<img src='", imageName(city[1, "city"], mini = TRUE), "'>"))
                   desc = paste(sapply(desc, paste, collapse = ""), collapse = "\n")
                 } else 
                   desc = paste("Temperature for", city, row["temp"], "in", mnth)
                 
                 addDescription(desc, parent = mark)
           })


               # Ground Overlay to show the time series curve of temperature across the 4 months.
       off = c(.5, .5)*3
       loc = as.numeric(city[1, c("longitude", "latitude")])
       box = c(north = loc[2] + off[2], south = loc[2] - off[2],
                east = loc[1] + off[1], west = loc[1] - off[1])       
       addGroundOverlay(name = city[1, "city"],
                        Icon = c(color = "#88FFFFFF", href = imageName(city[1, "city"])),
                        LatLonBox = box, parent = folder)

   })
 )
}

f(xmlRoot(doc)[["Document"]], TRUE)

 # Now we add a legend to provide information about the colors used in the time-series plots
 # We make this a ScreenOverlay so that it remains fixed. If we wanted, we could allow it to move
 # and so put it at a point in the sea, for examle.
 # We display the legend in the lower left corner.
 # The legend should have no margins in R.
addScreenOverlay(doc, 
                 name = "Temperature legend",
                 Icon = "cityTemperatures/legend.png",
                 screenXY =  c("x" = 2, "y" = 2, xunits = "pixels", yunits = "pixels"),
                 overlayXY = c("x" = 0, "y" = 0),
                 size = c("x" = 150, "y" = 250, xunits = "pixels", yunits = "pixels"))

print(saveXML(doc, "doc.kml"))
#system("open /tmp/doc.kml")
