kmlPolygon =
function(long, lat, col = character(), fill = FALSE, lwd = 1, outline = 1,
         style = list(PolyStyle = list(color = col, fill = fill, width = lwd, outline = outline)),
         parent = NULL, localStyle = missing(style) || !is.character(style))
{
  len = length(long)
  if(long[1] != long[len] || lat[1] != lat[len]) {
      long = c(long, long[1])
      lat = c(lat, lat[1])
  } 

  
  coords = paste(long, lat, 0, sep = ", ", collapse = " ")
  place = newXMLNode("Placemark", parent = parent,
                      newXMLNode("MultiGeometry",
                                   newXMLNode("Polygon",
                                                newXMLNode("outerBoundaryIs",
                                                              newXMLNode("LinearRing", 
                                                                  newXMLNode("coordinates",  coords))))))

#  if(is.null(parent))
#    parent = place

  if(length(style)) {
    id = character()
    
    if(is.list(style)) {
       if(!is.null(parent)) { # always TRUE now.
#         id = names(style)
         if(length(id) == 0)
             id = "polyLineStyle"

         if(is.null(parent) || localStyle)
            makeStyle(style, parent = place)
         else
            addStyles(as(parent, "XMLInternalDocument"), list(polyLineStyle = style))
       }
    } else
        id = style

     if(length(id))
        newXMLNode("styleUrl", id, parent = place)
  }

  place
}

  
