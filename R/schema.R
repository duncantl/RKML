#  http://code.google.com/apis/kml/schema/kml21.xsd

createLookAt =
  #
  # This creates a LookAt node.
  # 
  # latitude, longitude, altitude, tilt, heading
  #
function(window, parent)
{
  if(length(window) == 0)
    return(NULL)
  
  lookat = newXMLNode("LookAt", parent = parent)

  if(! ( "altitudeMode" %in% names(window)))  
    window["altitudeMode"] = "absolute" #absolute - relative to sea level

  if(! ( "altitude" %in% names(window)))
    window["altitude"] = 4000000  # meters
  
  sapply(names(window),
         function(id)
          newXMLNode(id, window[id], parent = lookat))
  lookat
}

