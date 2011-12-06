# Ground overlay

groundOverlay =
function(icon, long, lat, width, height, rot = 0, ..., name = character(), parent = NULL)
{
  go = newXMLNode("GroundOverlay", parent = parent)

  if(length(name) && !is.na(name))
    newXMLNode("name", name, parent = go)

  dims = c(west = long, south = lat, east = long + width, north = lat + height)
  dims = dims[c("north", "south", "east", "west")]
  
  box = newXMLNode("LatLonBox", parent = go)
  mapply(newXMLNode, names(dims), dims, MoreArgs = list(parent = box))

  if(rot != 0)
    newXMLNode("rotation", rot, parent = box)

  newXMLNode("Icon", newXMLNode("href", icon), parent = go)

  go
}
