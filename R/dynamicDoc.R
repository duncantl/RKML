createUpdatingDoc =
#
# Create a KML document that regularly reloads another.
# Optionally write this document to a file whose name is given by out.

function(targetFile, interval = 5, window = c(),
         docName = targetFile,
         description = sprintf("Updating of %s file", targetFile),
         out = NA, ...,
         doc = createKMLDoc(targetFile, description, window, ...))
{

  net = newXMLNode("NetworkLink", parent = xmlRoot(doc)[["Document"]],
                     newXMLNode("open", 1))
  newXMLNode("Link", newXMLNode("href", path.expand(targetFile)),
                     newXMLNode("refreshMode", "onInterval"),
                     newXMLNode("refreshInterval", interval),
                     newXMLNode("viewRefreshMode", "onStop"),
                     newXMLNode("viewRefreshTime", interval),                                       
                parent = net)

  if(!is.na(out))
     saveXML(doc, out)
  else
     invisible(doc)
}
