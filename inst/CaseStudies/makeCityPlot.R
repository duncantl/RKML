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

