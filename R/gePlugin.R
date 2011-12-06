
setAPIKey =
function(key = getOption("GoogleEarthAPIKey", Sys.getenv("GOOGLE_EARTH_API_KEY")),
         doc = system.file("pluginTemplate", "gePluginTemplate.html", package = "RKML"))
{
  if(is.character(doc))
    doc = htmlParse(doc)

  nodes = getNodeSet(doc, "//head/script[contains(@src, '/jsapi?key=')]")
  if(length(nodes) == 0) {
     head = xmlRoot(doc)[["head"]]
     if(is.null(head))
       head = newXMLNode("head", parent = xmlRoot(doc), at = 0)
     src = sprintf("http://www.google.com/jsapi?key=%s", key)
     newXMLNode("script", attrs = c(src = src), parent = head)
  } else {
    node = nodes[[1]]
    src = xmlGetAttr(node, "src")
    newSrc = paste(gsub("/jsapi\\?key=.*", "/jsapi\\?key=", src), key, sep = "")
    xmlAttrs(node) = c(src = newSrc)
  }

  doc
}


insertJS =
function(doc, code, inline = is(code, "AsIs") || length(grep("var | function ", code)), 
          replace = FALSE)
{
  h = getNodeSet(doc, "//head")[[1]]

  if(is.logical(replace) && replace)
    replace = "geInitialization.js"

     # remove any reference to a <script src='file'>  if file is
  if(is.character(replace) && length(replace)) {
          # we could just get all the script[@src] nodes and then their @src
          # attributes and see which elements the replace vector matches.
	lapply(replace, function(src) {
	                   nodes = getNodeSet(h, sprintf(".//script[@src = '%s']", src))
                           if(length(nodes))
           	              removeNodes(nodes)
	               })
  }
  
  if(inline) {
    sc = newXMLNode("script", parent = h)
    if(!is(code, 'AsIs')) {
       xmlAttrs(sc) = c("filename" = code)
       code = readLines(code)
     }
    newXMLCDataNode(paste(code, collapse = "\n"), parent = sc)
  } else {
    lapply(code, function(src) {
                   sc = newXMLNode("script", parent = h)
	           xmlAttrs(sc) = c(rel = "text/javascript", src = src)
                  })
  }

  doc
}


dQuote =
  function(x)
     sprintf('"%s"', x)

copyPluginJSCode =
function(to = "geInitialization.js",  kml = character(),
         multipleGEs = FALSE,
         source = system.file("pluginTemplate",
                              if(multipleGEs)
                                "geMultiInitialization.js"
                              else
                                "geInitialization.js",
                              package = "RKML") ) 
{
   txt = readLines(source)

   if(length(kml) && !is.na(kml)) {
       # allow for multiple kml files. If there is more than one, does that
       # mean to load them into separate GE's or each into one GE.
       # What if they were in-memory XML documents.
     if(multipleGEs) {
         str = paste("{", paste(dQuote(names(kml)), dQuote(kml), sep = ": ", collapse = ", "), "}")
         i = grep("var KMLDocs =", txt)
         def = sprintf("var KMLDocs = %s;", str)
         if(length(i)) {
           txt[i] = gsub("var KMLDocs = .*", def, txt[i])
         } else {
           txt = c(def, txt)
         }
     } else {
       i = grep("google.earth.fetchKml(ge, 'XXX', fetchCallback);", txt)
       if(length(i)) {
         txt[i] = gsub("XXX", kml, txt[i])
       } else {
           # where to insert it?
       }
     }
   }
   
   if(!is.na(to))
      cat(txt, file = to, sep = "\n")
   else
     invisible(txt)
}
