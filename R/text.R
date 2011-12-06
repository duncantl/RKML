kmlText =
function(long, lat, labels, ...,
          icon = system.file("Icons", "empty.png", package = "RKML"),
           parent = NULL)
{


             newXMLNode("Style", attrs = c(id = "empty"),
                          newXMLNode("IconStyle",
                                      newXMLNode("Icon", icon)),
                         parent = parent)

             invisible(
                mapply(function(x, y, txt) {
                        newXMLNode("Placemark",
                                      newXMLNode("name", txt),
#                                      newXMLNode("Icon",  newXMLNode("href", icon)),
                                      newXMLNode("styleUrl", "#empty"),
                                      newXMLNode("Point",
                                                      newXMLNode("coordinates", sprintf("%.4f, %4f, 0", x, y))),
                                    parent = parent)
                     }, long, lat, labels))
}  

#XXX fix this up so that the doc doesn't have to be passed as the first argument.
setMethod("text", "KMLDoc",
          function(x, long, lat, labels, ..., icon = system.file("Icons", "empty.png", package = "RKML"),
                     parent = xmlRoot(x)[["Document"]]) {
             kmlText(long, lat, labels, ..., icon = icon, parent = xmlRoot(x)[["Document"]])
           })
