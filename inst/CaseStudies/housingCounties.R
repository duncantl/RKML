
library(RKML)
library(XML)

if(!exists('housing')) {
  load("~/Data/housing.Rda")

  housing$city[ housing$city == "`vallejo" ] = "Vallejo"
  housing$city[ housing$city == "`san Rafael" ] = "San Rafael"
  housing = subset(housing, city != "")
  housing$city = factor(as.character(housing$city))
}


locations = by(housing, housing$city,
                   function(x)
                      c(median(x$long, na.rm = TRUE), median(x$lat, na.rm = TRUE)))


overall.prsqft = with(housing, by(price/bsqft, cut(date, "weeks"), median, na.rm = TRUE))
overall.dates = as.Date(gsub(" .*", "", levels(cut(housing$date, "weeks"))), "%Y-%m-%d")


colors = rainbow(10) # max(housing$br, na.rm = TRUE))

makeCityPlot =
function(x, ylim = range(housing$price/housing$bsqft, na.rm = TRUE), 
          addBedroomSeries = FALSE, toFile = TRUE)
{
   # we may have too little data, i.e. only one week or al NAs for the prices or bsqft
  when = cut(x$date, "weeks")
  if(length(levels(when)) < 2)
    return(NA)
       
  price = by(x$price/x$bsqft, when, median, na.rm = TRUE)
  if(all(is.na(price)))
    return(NA)

   # arrange to draw to a file if toFile is TRUE, otherwise the screen
  cityName = as.character(x$city[1])
  if(toFile) {
    fname = sprintf("%s.png", gsub("[ /]", "_", cityName))
    png(fname)
    on.exit(dev.off())
  } else
    fname = NA

    # Draw the plot for the median price/sqft for this 
  dates = as.Date(gsub(" .*", "", levels(when)), "%Y-%m-%d")
  plot(dates, price, type = if(any(is.na(price))) "o" else "l",
       xlab = "Date", ylab = "$ per square foot", main = cityName,
       ylim = ylim, lwd = if(addBedroomSeries) 2 else 1)

  lines(overall.dates, overall.prsqft, lty = 3, col = "green")
  rug(as.Date(x$date), col = "red")

    # Draw a time series of median price/sqft for houses with the same number of bedrooms
    # and do this for the different number of bedrooms.
  if(addBedroomSeries) {
    x = subset(x, !is.na(br) & br <= length(colors))
    by(x, x$br,
       function(pr) {
         if(nrow(pr) == 1 || length( w <- cut(pr$date, "weeks") ) == 0)
           return()
            # compute the median price for each week
         vals = by(pr$price/pr$bsqft, w, median, na.rm = TRUE)

         lines(as.Date(gsub(" .*", "", levels(w)), "%Y-%m-%d"), vals, lty = "dashed", col = colors[pr$br[1]])
       })
  }

  fname
}


pics = by(housing, housing$city,
           makeCityPlot,
          ylim = c(0, with(housing, quantile(price/bsqft, .95, na.rm = TRUE))),
          addBedroomSeries = FALSE)


any(is.na(pics))
locations[!is.na(pics)]


doc = createKMLDoc("City Housing Prices", "Median housing prices per square foot for each city for each week",
                    window = c(longitude = median(housing$long, na.rm = TRUE),
                               latitude = median(housing$lat, na.rm = TRUE)))


folder = newXMLNode("Folder", newXMLNode("name", "Cities"), parent = xmlRoot(doc)[["Document"]])


makePlacemark =
function(img, loc, cityName, count, folder = NULL)
{
  if(is.na(img))
    return(NULL)
  pl = newXMLNode("Placemark", parent = folder)
  newXMLNode("name", cityName, parent = pl)
  desc = sprintf('<h2>%s</h2>\nNumber of houses sold: %d<br/><img src="%s">', cityName, count, img)
  newXMLNode("description", newXMLCDataNode(desc), parent = pl)
  newXMLNode("Point",
              newXMLNode("coordinates", paste(loc[1], loc[2], 0, sep = ",")),
             parent = pl)
  pl
}


invisible(mapply(makePlacemark, pics, locations, names(locations), table(housing$city), MoreArgs = list(folder)))


saveXML(doc, "cityHousePrices.kml")

