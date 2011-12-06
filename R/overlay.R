addGroundOverlay =
  #
  # see groundOverlay for a more R-friendly function.
  #
function(doc, ..., parent = xmlRoot(doc)[["Document"]],
         node = newXMLNode("GroundOverlay", parent = parent))
{

  args = list(...)
  sapply(names(args),
         function(id) {
           val = args[[id]]
           tmp = newXMLNode(id, parent = node)
           if(id == "LatLonBox" || length(names(val))) {
              mapply(newXMLNode, names(val), val,
                          MoreArgs = list(parent = tmp))             
           } else
             addChildren(tmp, newXMLTextNode(val))
         })
  node
}


addScreenOverlay =
function(doc, ..., parent = xmlRoot(doc)[["Document"]],
         node = newXMLNode("ScreenOverlay", parent = parent))
{

  useAttributes = c("screenXY", "overlayXY", "size")
  
  args = list(...)
  sapply(names(args),
          function(id) {

             el = newXMLNode(id, parent = node)
             if(id %in% useAttributes)
               addAttributes(el, .attrs = args[[id]])
             else
               addChildren(el, args[[id]])

          })
   node
}

addPlacemark =
function(point, name, description = character(), id = character(),
          styleUrl = character(), time = NA, parent = NULL)
{
  node =  newXMLNode("Placemark", newXMLNode("name", name), parent = parent)
  if(length(id) && !is.na(id))
    addAttributes(node, id = id)

  if(length(description) && !is.na(description))
    newXMLNode("description", newXMLCDataNode(paste(description, collapse = "")), parent = node)

  if(length(styleUrl) && !is.na(styleUrl))
    newXMLNode("styleUrl", styleUrl, parent = node)  

  if(length(point))
    newXMLNode("Point", newXMLNode("coordinates", paste(point, collapse = ", ")), parent = node)    

  if(length(time) && !is.na(time))
    addTimeStamp(time, parent = node)
  
  node
}

# Make this generic.
# Handle adding to an XML document or a KMZ file.
# Dispatch on parent.
# Match the signature with ROOXML::addImage.

addImage = addIcon =
function(file, color = character(), parent = NULL)  
{
  node = newXMLNode("Icon", newXMLNode("href", file), parent = parent)
  if(length(color) && !is.na(color))
    newXMLNode("color", color, parent = node)

  node
}

addTimeStamp =
function(when, parent = NULL, format = "%Y-%m-%dT%H:%M:%S%Z")
{
  UseMethod("addTimeStamp")
}

addTimeStamp.default =
function(when, parent = NULL, format = "%Y-%m-%dT%H:%M:%S%Z")
  newXMLNode("TimeStamp", newXMLNode("when", when), parent = parent)


addTimeStamp.POSIXlt = addTimeStamp.POSIXct =
function(when, parent = NULL, format = "%Y-%m-%dT%H:%M:%S%Z")
{  
  addTimeStamp(format(when, format), parent)
}


addTimeStamp.Date =
function(when, parent = NULL, format =  "%Y-%m-%d")
{  
  addTimeStamp(format(when, format), parent = parent)
}


addDescription =
function(text, parent = NULL, cdata = TRUE, sep = "")
{
  node = newXMLNode("description", parent = parent)

  text = paste(text, collapse = sep)
  addChildren(node, if(cdata)   newXMLCDataNode(text)  else   text)
  node
}

