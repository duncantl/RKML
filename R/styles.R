makeStyle =
  #
  # Given a list whose names identify XML nodes and whose contents
  # are also nodes within that node.
  #
  #  makeStyle(list(LineStyle = c(color = "ff0000", width = 2)))
  #  makeStyle(list(LineStyle = c(color = "ff0000", width = 2), geomCol = "ffffff"))
  #
function(attrs, id = NA, parent = NULL)
{
  style = newXMLNode("Style", attrs = if(!is.na(id)) c(id = id) else character(), parent = parent)
  sapply(names(attrs),
         function(name) {
           node = newXMLNode(name, parent = style)
           if(length(names(attrs[[name]])))
               mapply(makeStyleAttr, names(attrs[[name]]), attrs[[name]], MoreArgs = list(parent = node))
           else
             newXMLTextNode(attrs[[name]], parent = node)
           node
         })
  style
}

makeStyleAttr =
function(name, value, parent = NULL)
{
  if(length(value) == 0)
    return(NULL)
  
  if(name == "color" && !inherits(value, "AsIs"))
      value = rgbToKMLColor(value)
  
  newXMLNode(name, value, parent = parent)
}
