KML.ns = c(kml = "http://earth.google.com/kml/2.2")


kml =
function(x, data, folderName = deparse(substitute(data)), docDescription = deparse(substitute(x)), ...)
  UseMethod("kml")

kml.formula =
function(x, data,  folderName = deparse(substitute(data)), docDescription = deparse(substitute(x)), ...)
{
  kml(kmlFormula(x), data, folderName = folderName, docDescription = docDescription, ...)
}

kml.data.frame =
function(x, data, folderName = deparse(substitute(data)), docDescription = folderName, ...)
{
    # Find the variables.
  i = pmatch(c("lat", "lon", "time"), tolower(names(x)))
  if(any(is.na(i[1:2])))
      stop("Can't identify longitude and latitude unabmiguously")

  names(x)[i[1:2]] = c("latitude", "longitude")

  if(is.na(i[3])) {
    isDate = sapply(x, function(x) any(inherits(x, c("Date", "POSIXt"))))
    if(sum(isDate) == 0)
       stop("Cannot find a date/time variable")
    else if(sum(isDate) > 1)
       stop("Cannot identify date/time variable unambiguously")

      names(x)[isDate] = "time"    
  } else
    names(x)[i[3]] = "time"

    
  if("time" %in% names(x))
     kmlTime(x, x[["time"]], folderName = folderName, docDescription = docDescription, ...)
  else
     kmlPoints(x, data, folderName = folderName, docDescription = docDescription, ...)
}



#XXX We probably want an additional parameter which is the parent.frame()
# of the actual call to kml.
kml.KMLFormula =
function(x, data, folderName = deparse(substitute(data)), docDescription = folderName, times = NULL, ...)
{
  if("time" %in% names(x) || !missing(times)) {
     needTime = missing(times)
     ids = c(x$LongLat, if(needTime) c(time = x$time))

 # Evaluate the expressions.     
     vals =  lapply(ids, function(e) {
                           if(is(e, "TimeSpanSpec")) {
                              ans = new("TimeSpan", list(end = eval(e@end, data, parent.frame()),
                                                   start = eval(e@start, data, parent.frame())))
                              ans@row.names = rownames(data)
                              ans
                           } else if(is(e, "TimeSpec")) {
                              eval(e@orig, data, parent.frame())

                           } else
                              eval(e, data, parent.frame())
                           })

     if(needTime)
       times = vals[[ "time" ]]
     
     return(kmlTime(data, times, folderName = folderName, docDescription = docDescription, ..., 
            .longitude = vals$long, .latitude = vals$lat))
  }

# ids = x$LongLat
# i = match(ids, names(data))
# if(any(is.na(i)))
#     stop("Cannot find ", paste(ids[is.na(i)], collapse = ", "), " in data frame")
#  names(data)[i] = c("longitude", "latitude")

  vals =  lapply(x$LongLat, function(e) eval(e, data, parent.frame()))
  if(length(x$groups))
    by = eval(x$groups, data, parent.frame())
  else
    by = integer()
  kmlPoints(data, folderName = folderName, docDescription = docDescription, ..., 
            .longitude = vals$long, .latitude = vals$lat, panelVar = by)
}



kmlTime =
#
# groups argument treats different subsets of the data and times as
#   separate traces.
#  
# Support a formula interface.
#  
#   to identify the lat, long, altitude and time
#     perhaps functions of a variable to give color

#  Look at zoo and other time series packages for a formula
#  language for time and also ggplot2 and Grammar of Graphics.  
#  Look at CRAN Task Views
#
#     ~ long + lat @ time | conditioning variables
#
# This is tricky to parse since the @time is bound to lat, not long + lat
# But if there is only one time variable, this is not a terrible drawback.
#
# See formula.R
#
# A panel-like function for working on the different  groups.
#   Takes
#     data  subset of data of interest
#     times vector of time/date values corresponding to data
#     folderName - string giving name of new folder to create.
#     doc - XMLInternalElement <Document>,
#     ids  color and line styles in a list Style ids.
#
function(data, times = data$time, style = character(), lty = character(),
         docName = deparse(substitute(data)),
         docDescription = NA,
         folderName = deparse(substitute(data)),
         window = c(longitude = mean(.longitude, na.rm = TRUE), latitude = mean(.latitude, na.rm = TRUE),
                      altitude = 4100000, tilt = 0, heading = 0),
         docStyles = list(), groups = character(),
         panel = kmlPanel.default, ...,
         kml.doc = createKMLDoc(docName, docDescription, window, data),
         .longitude = data$long,
         .latitude = data$lat,
         ids = if(length(names(.longitude))) names(.longitude) else rownames(data))
{
  .pos = data.frame(longitude = .longitude, latitude = .latitude)
  rownames(.pos) = ids

  if(missing(docName))
     docName = deparse(substitute(data))

  if(missing(times) && is.null(times)) {
     vars = tolower(names(data))
     i = pmatch(c("time", "date"), vars, 0)
     if(all(i == 0))
       stop("Can't identify the time variable")
     times = data[[ vars[i][1] ]]
  }

#  if(!inherits(times, "POSIXt"))
#    stop("times must be a POSIXt object")

  if(length(groups)) {
    if(is.character(groups) && length(groups) == 1)
      groups = data[[groups]]

    i = is.na(groups)
    if(any(i)) {
     groups = as.character(groups)
     groups[i] = "Missing value (NA)"
    }
  }

  doc = xmlRoot(kml.doc)[[1]]

  if(!is.character(docStyles))
     addStyles(doc, docStyles)
  else if(!inherits(docStyles, "AsIs")) {
           #XXX need to see if they are already there, at least for some.
     if (length(style) == 0)
         style = docStyles
  }

  allStyles = anchorStyles(col = style, lty = lty)

  #XXX Make this based on the conditioning not the groups.
  # Groups are styles within the panel/folder.
  if(length(groups) == 0) {
     panel(.pos, times, folderName, doc, 
          list(style = rep(allStyles$col, nrow(.pos)), 
               lty = rep(allStyles$lty, nrow(.pos))), ...)
  } else {
    groups = if(!is.factor(groups)) factor(groups) else groups
    
    for(i in seq(along = levels(groups))) {
      groupName = levels(groups)[i]
      w = groups == groupName
      n = sum(w)
      panel(.pos[w,], times[w], groupName, doc, 
         list(style = rep(allStyles$col[w], n), 
              lty = rep(allStyles$lty[w], n)), ...)
    }
  }

  invisible(kml.doc)
}

##################


#XXXX Do we have a generic kmlPanel?

kmlPanel.default =
function(data, times, folderName, doc, styles, ...)
{
    folder = newXMLNode("Folder", newXMLNode("name", folderName), parent = doc)
    addChildren(doc, addDataFolder(data, times, folder, styles$style, styles$lty, ...))
}

addDataFolder =
function(data, times, folder = newXMLNode("Folder", if(length(id) > 0) attrs = c(id = id)),
         col = 'red', lty = 'a',
         id = character(),
         addLines = FALSE,
         name = if(length(base:::names(times))) base:::names(times) else times,
         description = name,
         dateFormat = "%Y-%m-%dT%H:%M:%S") 
{
  col = rep(col, length = nrow(data))
  lty = rep(lty, length = nrow(data))


  if(is.function(dateFormat))
     times = dateFormat(times)
  else if(inherits(times, "POSIXt"))
      times = format(times, dateFormat)
  else if(!is(times, "TimeSpan"))
      times = as.character(times)

  addFolderPlacemarks(folder, data, times, description, name, col, lty, addLines)
  
  folder
}


addFolderPlacemarks =
function(folder, data, times, description, name, col, lty, addLines = FALSE)
{
# Deb - note that there is no id argument to this function.
# Deb - note that col and lty are separate arguments rather than a single styles argument.
  col[is.na(col)] = ""
  i = col != ""
  col[i] = sprintf("<styleUrl>%s</styleUrl>", col[i])

  timeStr = as(times, "KMLTime")
  
  if(addLines) {
    lty[is.na(lty)] = ""
    i = lty != ""
    lty[i] = sprintf("<styleUrl>%s</styleUrl>", lty[i])
    n = nrow(data)

    lines = sprintf("<Placemark>%s%s<LineString><tessellate>1</tessellate><extrude>0</extrude><coordinates>%.3f,%.3f,0 %.3f,%.3f,0</coordinates></LineString></Placemark>",
                    if(length(lty) > 1) lty[-n] else lty,
                    timeStr[-n],
                    data$longitude[ -n], data$latitude[-n],
                    data$longitude[-1], data$latitude[-1])
  } else
    lines = character()


  txt = sprintf("<Placemark>\n<name>%s</name><description><![CDATA[%s]]></description>%s%s<Point><coordinates>%.3f,%.3f,0</coordinates></Point></Placemark>",
            name, description, timeStr, col, data$longitude, data$latitude)

  parseXMLAndAdd(c(txt, lines), folder)
  folder
}

parseXMLAndAdd =
function(txt, parent)
{
  doc = xmlParse(sprintf("<tmp>%s</tmp>", paste(txt, collapse = "")), asText = TRUE)
  .Call("R_insertXMLNode", xmlChildren(xmlRoot(doc)), parent, -1L, FALSE,  PACKAGE = "XML")  
}


# Deb - Remove the col and lty arguments
# function(doc, styles, col = character(), lty = character())

addStyles =
function(doc, docStyles)
{
  if(inherits(doc, "XMLInternalDocument"))
    doc = xmlRoot(doc)

  if(xmlName(doc) == "kml")
    doc = doc[["Document"]]
  
    # Add the styles if there are any.
  if(all(sapply(docStyles, inherits, "XMLInternalNode")))
     addChildren(doc, kids = docStyles)
  else
     docStyles = mapply(makeStyle, docStyles, names(docStyles), MoreArgs = list(doc))

return(TRUE)
# Deb - remove the call to anchorStyles
#  ids = anchorStyles(col, lty, styles)
}


createKMLDoc =
  #
  #
  #
function(docName, description, window = c(longitude = 0, latitude = 0),
          data = NULL, ns = character()) # KML.ns)
{
  kml.doc = newXMLDoc(, KML.ns, name = "kml")
  kml = newXMLNode("kml", doc = kml.doc, namespaceDefinitions = ns, namespace = names(ns))

  doc = newXMLNode("Document", parent = kml)
  newXMLNode("name", docName, parent = doc)
     # If there is a description, put it in, either as the contents of the file or as is.
  if(!is.na(description)) {
     if(inherits(description, "connection") || (!inherits(description, "AsIs") && file.exists(description)))
        description = paste(readLines(description), collapse = "\n")

     newXMLNode("description", newXMLCDataNode(description), parent = doc)
   }

  if(length(window) && length(names(window)) == 0)
    names(window) = c("longitude", "latitude")

  if(length(data)) {
    if(is.na(window["latitude"]))
        window["latitude"] = mean(data$latitude, na.rm = TRUE)
    if(is.na(window["longitude"]))
        window["longitude"] = mean(data$longitude, na.rm = TRUE)
  }
  
  createLookAt(window, doc)

  structure(kml.doc, class = extends("KMLDoc"))
}

# Deb - remove the styles argument
# function(col, lty, styles)
# If a hash exists in the reference then don't add it 
# (it need not be at the beginning) 

anchorStyles =
  #
  # Added groups, data and docStyles
  #
  #
function(col, lty, groups = integer(), data = NULL, docStyles = NULL)
{
  if(length(col)) {
     addHash = !grepl("#", col)
     col[addHash] = paste("#", col[addHash], sep="")
  }
  if(length(lty)) {
     addHash = !grepl("#", lty)
     lty[addHash] = paste("#", lty[addHash], sep="")
  }

  if(length(col) == 0 && (length(groups) && length(docStyles))) {
     if(is.factor(groups) && all(levels(groups) %in% names(docStyles))) {
        i = match(as.character(groups), names(docStyles))
        col = names(docStyles)[i]
     } else
        col = names(docStyles)[as.factor(groups)]
  }
  
  list(col = col, lty = lty)
}

addFolder =
function(name, id = character(), parent = NULL)
{
  if(is(parent, "KMLDoc"))
     parent = xmlRoot(parent)[["Document"]]
  node = newXMLNode("Folder", newXMLNode("name", name),  parent = parent)
  if(length(id) && !is.na(id))
     addAttributes(node, id = id)

  node
}


###########################################################
if(FALSE)  # not used any more. 
addFolderPlacemarks =
function(folder, data, times, descriptions, names, col, lty, addLines = FALSE)
{
  for (i in 1:nrow(data)) {

    time = times[i]

    pm = newXMLNode("Placemark", parent = folder)
      newXMLNode("name", names[i], parent = pm)
      newXMLNode("description", newXMLCDataNode(descriptions[i]), parent = pm)
        tsp = newXMLNode("TimeSpan",
                         newXMLNode("begin", time),
                         newXMLNode("end", time),
                         parent = pm)
        if(length(col) && !is.na(col[i]) && col[i] != "")
            newXMLNode("styleUrl", col[i], parent = pm)
        pt = newXMLNode("Point", parent = pm)
             newXMLNode("coordinates", 
                          paste(data$longitude[i], ",", data$latitude[i], ",0", sep =""), parent=pt)

          # Connecting path
         if (addLines && i < nrow(data) ) {
            pm = newXMLNode("Placemark", newXMLNode("styleUrl", lty[i]), parent = folder)
                   
            tsp = newXMLNode("TimeSpan",
                                newXMLNode("begin", time),
                                newXMLNode("end", time),
                              parent = pm)

            if(addLines) {
              lstr = newXMLNode("LineString", parent = pm)
              newXMLNode("tessellate", "1", parent = lstr)
              newXMLNode("extrude", "0", parent = lstr)
              newXMLNode("coordinates", 
                               paste( data$longitude[i], ",", data$latitude[i], ",0 ", 
                                       data$longitude[i+1], ",", data$latitude[i+1], ",0 ", 
                                      sep =""), parent = lstr)
            }
         }
  }
}


# Use Faster version of newXMLNode with very little checking and fewer features.
if(FALSE)
newXMLNode =
  #
  #  f = newXMLNode("foo")
  #  newXMLNode("bar", parent = f)
  #  newXMLNode("other", attrs = c(a = 1, b = 2), parent = f)
  #
function (name, ..., attrs = NULL, namespace = character(), namespaceDefinitions = character(), 
    doc = NULL, .children = list(...), parent = NULL, at = NA, 
    cdata = FALSE, suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning", 
        FALSE), sibling = NULL, manageMemory = TRUE) 
{

      node <- .Call("R_newXMLNode", as.character(name), character(), 
                        character(), doc, namespaceDefinitions, as.integer(manageMemory), 
                       PACKAGE = "XML")
     if (!is.null(parent))
        .Call("R_insertXMLNode", node, parent, -1L, FALSE,  PACKAGE = "XML")

      if (length(.children)) {
             # or weaker test but maybe faster typeof(i) == "externalptr"
             # .Call("R_insertXMLNode", if(inherits(i, "XMLInternalNode")) i else newXMLTextNode(i), node, -1L, FALSE,  PACKAGE = "XML")
           .Call("R_insertXMLNode", .children, node, -1L, FALSE,  PACKAGE = "XML")
      }

     .Call("RS_XML_addNodeAttributes", node, attrs, PACKAGE = "XML")

      node
}

