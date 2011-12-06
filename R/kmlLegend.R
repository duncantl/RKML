kmlLegend =
  #
  # Draw a legend.
  #
function(x = 0, y = 0, legend, ...,
         parent = NULL, name = "legend", units = rep("pixels", 2), dims = c(100, length(legend) * 12 * 1.5),
         filename = "legend.png", bg = "transparent")
{
  if(is.character(legend) && inherits(legend, "AsIs") && length(legend) == 1) {
      icon = legend
         # read the dimensions
      if(missing(dims)) {
         dims = readPNGDims(legend)
      }
  } else {
       # Draw the icon ourselves by creating a graphics device just for the legend.
     icon = "legend.png"

     opar = par(no.readonly = TRUE)     
     on.exit(par(opar))     #??? Should we not reset???
     png(icon, dims[1], dims[2], bg = bg)     
     par(mar = c(0, 0, 0, 0))
     plot(0, type = "n", xlim = c(0,1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab  = "", main = "")
     legend(0, 1, legend, ...) # c("A", "B", "C"), lty = c(1, 2, 3), col = c("red", "green", "blue"))  
     dev.off()
  }


  if(length(units) == 1)
     units = rep(units, 2)

  if(inherits(parent, "KMLDoc"))
     parent = xmlRoot(parent)[["Document"]]
  
  ov = newXMLNode("ScreenOverlay",
                   newXMLNode("name", name),
                   newXMLNode("Icon", newXMLNode("href", icon)),
                   newXMLNode("screenXY",  attrs = c(x= x,  y = y,  xunits = units[1], yunits = units[2])),
                   newXMLNode("overlayXY",  attrs = c(x= 0,  y = 0,  xunits = "fraction", yunits = "fraction")),
                   newXMLNode("size", attrs = c(x = dims[1], y = dims[2], xunits = "pixel", yunits = "pixel")),
                  parent = parent)
}

# We could use a package or some C code to read a PNG file,
# but 

readPNGDims =
function(filename)
{
  con = file(filename, "rb")
  on.exit(close(con))
  readBin(con, raw(), 16)  
  w = readBin(con, raw(), 4)
  h = readBin(con, raw(), 4)
  c(bytesToInt(w), bytesToInt(h))
}

bytesToInt =
function(x)
{
  sum(as.integer(x) * (256^(3:0)))
}
