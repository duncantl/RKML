# How to draw a circle in KML

kmlCircle =
function(x, y, r, parent = NULL, numLines = 60,
          col = NA, color = character(), scale = 1L, ..., styleId = NA) # numLines should be a function of radius
{
   if(missing(y) && length(x) == 2) {
       y = x[2]
       x = x[1]
   }

   if(!is.null(parent) && (!missing(col) || !missing(color) || length(list(...)))) {
       attrs = list(...)
       attrs$geomCol = color
       attrs$geomScale = scale
       
       makeStyle(attrs, NA, parent = parent)
   }
      
   angle = seq(0, 2*pi, length = numLines)
   xcoords = as.numeric(x) + r*cos(angle)
   ycoords = as.numeric(y) + r*sin(angle)   

   coords = paste(xcoords,  ycoords, 0, sep = ",", collapse = " ")
   newXMLNode("LineString", if(!is.na(styleId)) newXMLNode("styleUrl", paste("#", styleId, sep = "")),
                            newXMLNode("coordinates", coords), parent = parent)
}
