library(RKML)
data("temperature")

temperature$longitude = - temperature$longitude

  # Add a qualitative descriptor for the temperature
  # We'll use these to color the line segments to indicate
  # the category in to which the value falls.
temperature$desc = cut(temperature$temp, 4)

   # make the individual plots, one for each city.
   # Here are the colors we will use to represent the different categories
   # of temperature.
colors = c("blue", "green", "orange", "red")

dir.create(paste(c("tests", "images"), collapse = .Platform$file.sep), recursive = TRUE)

makePlot =
function(x, usePNG = TRUE) {
   if(usePNG) {
     img = sprintf("tests/images%s%s.png", .Platform$file.sep, as.character(x$city[1]))
     png(img, 300, 300, bg = "transparent")
     on.exit(dev.off())
   }
     
   plot(2, type = "n", xlim = range(as.integer(x$month)), ylim = range(temperature$temp),
         axes = FALSE, xlab = "", ylab = "")
   axis(2)

    # Draw segments

   xs = seq(1, by = .5, length = 2 * nrow(x) - 1)
   ys = numeric(length(xs))
   i = seq(1, by = 2, length = nrow(x))

   ys[ i ] = x$temp
   ys[ (i + 1)[-length(i)] ] = (x$temp[1:(nrow(x) - 1)] + x$temp[-1])/2

   cols = rep(colors[x$desc], c(1, 2, 2, 2))
   segments(xs[-length(xs)], ys[ - length(ys) ], xs[-1], ys[-1], col = cols, lwd = 10)

   if(usePNG)
      img
}  


imgs = by(temperature, temperature$city, makePlot)

#####
# Now create the KML document with these images as GroundOverlay elements.

jan = subset(temperature, month == "January")

doc = createKMLDoc("Example of GroundOverlays", "Temperatures of US cities drawn as a time series for each city",
                    window = c(longitude = median(temperature$long),
                               latitude = median(temperature$lat)))

 # width and height are the sizes in degrees to give for each plot on the map.
width = 3
height = 3
mapply(function(long, lat, img, name, parent = NULL) {
          groundOverlay(img, long - width/2,  lat - height/2,  width, height,
                         name = name, parent = parent)
       }, jan$long, jan$lat, imgs, name = as.character(jan$city),
           MoreArgs = list(parent = xmlRoot(doc)[["Document"]]))

library(XML)
kdoc = xmlClone(doc)

print(saveXML(doc, "/tmp/cityTemps.kml"))

# createUpdatingDoc("cityTemps.kml", out = "ucity.kml")

