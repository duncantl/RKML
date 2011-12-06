kmlSegments =
  # add styles
function(x0, y0, x1, y1,
         col = character(), lty = integer(), lwd = integer(),
          ..., doc = NULL, parent = NULL)
{
 coords = paste(paste(x0, y0, 0, sep = ", ") , paste(x1, y1, 0, sep = ", "))

 lapply(coords,
         function(k) {
             line = newXMLNode("LineString", parent = parent)           
                 newXMLNode("coordinates", k, parent = line)
             line
         })
}
