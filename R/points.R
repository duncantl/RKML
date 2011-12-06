kmlPoints =
  #
  # support Icon, Description, etc. for each point.
  #
  #
function(data = NULL, style = character(), 
         docName = if(missing(data)) deparse(substitute(.longitude)) else deparse(substitute(data)),
         docDescription = NA,
         folderName = if(missing(data)) docName else deparse(substitute(data)),
         window = c(longitude = mean(.longitude, na.rm = TRUE),
                    latitude = mean(.latitude, na.rm = TRUE),
                    altitude = 4100000, tilt = 0, heading = 0),
         docStyles = I(""), # character(), 
         panel = kmlPanel.points,
         type = "p",  # See plot.default documentation. "p", "l", "o", "b", "c"
         groups = character(),
         ...,
         panelVar = NULL,
         kml.doc = createKMLDoc(docName, docDescription, window, data),
         .longitude = data$long, .latitude = data$lat,
         ids = if(length(names(.longitude))) names(.longitude) else rownames(data),
         .names = if("name" %in% names(data)) data$name else rownames(data),
         description = character(), col = character(), lty = character()
        )
{

#  if(missing(docName))
#     docName = deparse(substitute(data))

  doc = xmlRoot(kml.doc)[["Document"]]

  if (length(style) == 0) {
      style = col
  }
	
  if(!is.character(docStyles))
     addStyles(doc, docStyles)
  else if(!inherits(docStyles, "AsIs")) {
           #XXX need to see if they are already there, at least for some.
     if (length(style) == 0) 
         style = docStyles 
  }

  aStyle = anchorStyles(col = style, lty = character(), groups, data, docStyles)

  if(length(aStyle$col) && is.null(data$.col))
     data$.col = aStyle$col
  if(length(aStyle$lty) && is.null(data$.lty))
     data$lty = aStyle$lty  
  
    # Put .pos front and allow the entire data frame to be passed via a  ... element.
  .pos = data.frame(longitude = .longitude, latitude = .latitude)
  rownames(.pos) = ids


  if(length(panelVar)) {
          # lift this into the kmlPoints function if group is non-trivial.
          # If the caller gave us a col for each observation, use that.
          # If the caller gave us a col for each group, replicate the col
          # values according to group.
       panelVar = as.factor(panelVar)

if(FALSE) {  # temporarily skipping all in here as we try to compute the styles
       if(length(style) == 0){

         data$.col = names(docStyles)[panelVar]

       } else {
       
         if(length(aStyle) == nrow(data)) {
          data$.col = rep(aStyle, length = nrow(data))
         } else {
           data$.col = aStyle[panelVar]    
         }
       }

if(FALSE) {       
       if(length(lty) == nrow(data))
          data$.lty = rep(lty, length = nrow(data))
       else
          data$.lty = lty[panelVar]
} else
       data$.lty = rep(NA, nrow(data))
}

       if(missing(folderName))
          folderName = levels(panelVar)

data$.name = rep("", nrow(data)) # temporary

#       folder = newXMLNode("Folder", newXMLNode("name", folderName), parent = doc)
       by(1:nrow(data), panelVar,
              function(idx) {
                 sub = data[idx,]

                      # , folder = folder - used to create folder and pass it in.
                 panel(sub, as.character(panelVar[idx][1]), doc, sub$.name, styleUrl = sub$.col, 
    		        lty = sub$.lty, addLines = (type == "l"), .pos = .pos[idx,],
  	     	        name = .names[idx], description = description[idx], type = type)
                 })
       # return(folder)
       
  } else {  # no panel var
	  aStyle = lapply(aStyle, rep, length = nrow(.pos))

          force(folderName)
          if(missing(data))
            data = .pos
	  panel(data, folderName, doc, name = .names, styleUrl = aStyle, ..., 
                .pos = .pos, type = type, groups = groups, id = ids, 
                description = description)
  }
	

  invisible(kml.doc)
}

kmlPanel.points =
function(data, folderName = "data", doc,
         name = if("name" %in% names(data))  data[["name"]] else rownames(data),
         description = data[["description"]], styleUrl = data[["styleUrl"]], id = data[["id"]],
         .pos = cbind(data[, c("longitude", "latitude")]),
         addLines = !missing(lty) && length(lty) || type == "l", lty = "",
         type = "p", groups = character(),
         folder = if(!is.na(folderName))
                     newXMLNode("Folder", newXMLNode("name", folderName), parent = doc)
                  else
                     doc,
         ...)  #XXX adding ... to allow for unused parameters being passed to this function, but ignored.
{

  if (is.null(styleUrl))
    styleUrl = "" 
  styleUrl[is.na(styleUrl)] = ""

  if (length(id) > 0)
     id = sprintf("id='%s'", id)
  else
     id = ""

   if (length(description) == 0)
      description = ""
   if (all(is.na(description)))
      description = ""


  if(!is.logical(addLines)) {
     addLines = length(addLines) > 0
     lty = addLines
  }
  
 ans = addFolderPlacemarks.points(folder, as.data.frame(.pos), id, description, name, 
         col = if("col" %in% names(styleUrl)) styleUrl$col else styleUrl, 
         lty, addLines, groups)
}

addFolderPlacemarks.points =
function(folder, data, id, description, name, col, lty, addLines = FALSE, groups = character())
{
  
  if(length(col) > 0 && !all(col == "") && !all(is.na(col))) {
      i = !is.na(col) & col != ""
      plstyle = rep("", length(col))
      plstyle[i] = sprintf("<styleUrl>%s</styleUrl>", col[i])
   } else
      plstyle = ""

   desc = if(length(description) && (length(description) != 1 && description != ""))
             sprintf("<description><![CDATA[%s]]></description>", description)
          else
             ""
  
   txt = sprintf("<Placemark %s>\n<name>%s</name>%s%s<Point><coordinates>%.3f,%.3f,0</coordinates></Point></Placemark>",
                 id, name, desc, plstyle, 
                 data[, "longitude"], data[, "latitude"])

  if(addLines) {
     i = seq(1, nrow(data) - 1)

     if(length(lty) > 0 && !all(lty == "") && !all(is.na(lty))) {
        j = !is.na(lty) & col != ""
        lstyle = rep("", length(lty))
        lty[j] = anchorStyles(col = character(), lty = lty[j])$lty
        lstyle[j] = sprintf("<styleUrl>%s</styleUrl>", lty[j])
     }
     else
        lstyle = ""

     lines = c(sprintf("<Placemark>%s<LineString><extrude>0</extrude><tesselate>1</tesselate><coordinates>%.3f,%.3f,0 %.3f,%.3f,0</coordinates></LineString></Placemark>",
      lstyle[i], data[i, "longitude"], data[i, "latitude"], 
      data[i+1L, "longitude"], data[i+1L, "latitude"]),
               "")  # "" at the end is to get the right length

  txt = paste(txt, lines, sep = "")
  }

  ans = parseXMLAndAdd(txt, folder)
  folder
}

if (FALSE){
  #XXX may want to allow plstyle to be richer, with actual new styles defined in-line, etc. rather than
  #  just references to existing styles.
   if(length(styleUrl$col) > 0 && !all(styleUrl$col == "") && !all(is.na(styleUrl$col))) {
      i = !is.na(styleUrl$col) & styleUrl$col != ""
      plstyle = rep("", length(styleUrl$col))
      plstyle[i] = sprintf("<styleUrl>%s</styleUrl>", styleUrl$col[i])
   } else
      plstyle = ""
      desc = if(length(description)  && (length(description) != 1 && description != ""))
                sprintf("<description><![CDATA[%s]]></description>", description)
             else
                ""
   txt = sprintf("<Placemark %s>\n<name>%s</name>%s%s<Point><coordinates>%.3f,%.3f,0</coordinates></Point></Placemark>",
                 id, name, desc, plstyle, .pos[, "longitude"], .pos[, "latitude"])

  if(addLines) {
     i = seq(1, nrow(.pos) - 1)

     style = if(all(lty == "")) "" else {
                   if(!inherits(lty, "AsIs"))
                      lty = paste("#", lty, sep = "")
                   sprintf("<LineStyle>%s</LineStyle>", lty)
                }
     lines = c(sprintf("<Placemark>%s<LineString><extrude>0</extrude><tesselate>1</tesselate><coordinates>%.3f,%.3f,0 %.3f,%.3f,0</coordinates></LineString></Placemark>",
                       style, .pos[i, "longitude"], .pos[i, "latitude"], .pos[i+1L, "longitude"], .pos[i+1L, "latitude"]),
               "")  # "" at the end is to get the right length

     txt = paste(txt, lines, sep = "")     
  }  

   ans = parseXMLAndAdd(txt, folder)
}
