setOldClass(c("KMLDoc", extends("XMLInternalDocument")))

setGeneric("getHTMLLocalFileRefs",
            function(x, update = FALSE, ...)
                standardGeneric("getHTMLLocalFileRefs"))

tmp = 
function(x, update = FALSE, ...) {
               nodes = getNodeSet(x, "//description",
                                   c("kml" = "http://earth.google.com/kml/2.0"))
               sapply(nodes, getHTMLLocalFileRefs, update, ...)
            }

setMethod("getHTMLLocalFileRefs", "KMLDoc", tmp)
setMethod("getHTMLLocalFileRefs", "XMLInternalDocument", tmp)


setMethod("getHTMLLocalFileRefs", "character", 
            function(x, update = FALSE, ...) {
                # Could be a KML <description> node or a
              doc = htmlParse(x, asText = TRUE)
              if(xmlName(xmlRoot(doc)) == "kml")
                 class(doc) = extends("KMLDoc")
              
              getHTMLLocalFileRefs(doc, FALSE, ...)
            })


setMethod("getHTMLLocalFileRefs", "HTMLInternalDocument", 
            function(x, update = FALSE, ...) {
              getHTMLLocalFileRefs(xmlRoot(x)[["body"]], update, ...)
            })


setMethod("getHTMLLocalFileRefs", "XMLInternalElementNode", 
            function(x, update = FALSE, ...) {
                # Could be a KML <description> node or a

              if(xmlName(x) == "body") {
                 els = getNodeSet(x, "//img/@src | //a/@href")
                 return(unlist(els[sapply(els, isLocalFile)]))
              }
              else if(xmlName(x) == "description") {
                 if(xmlSize(x) == 0)
                    return(character())
                 else if(xmlSize(x) == 1 && is(x[[1]], "XMLInternalCDataNode")) {
                     txt = xmlValue(x[[1]])
                     if(length(txt) == 0 || txt == "")
                       return(character())
                     hdoc = htmlParse(xmlValue(x[[1]]), asText = TRUE)
                 } else 
                     hdoc = htmlParse(paste(xmlSApply(x, saveXML), collapse = ""), asText = TRUE)

                 hdoc = xmlRoot(hdoc)[["body"]]
              } else
                   hdoc = x

#              xmlName(x) == "description" || xmlName(x) == "text")
              html =  hdoc   # xmlRoot(htmlParse(xmlValue(x), asText = TRUE))[["body"]]
              ans = getHTMLLocalFileRefs(html, update, ...)
              if(update) {

              }
              ans
            })

setMethod("getHTMLLocalFileRefs", "XMLInternalCDataNode", 
            function(x, update = FALSE, ...) {
                # Could be a KML <description> node or a
               bdy = xmlRoot(htmlParse(xmlValue(x), asText = TRUE))[["body"]]
               getHTMLLocalFileRefs(bdy, update, ...)
            })


isLocalFile =
function(x)
{
   !grepl("^[[:space:]]*(http|ftp)", x)
}  
  

setGeneric("folderNames", 
           function(x, ...) {
             standardGeneric("folderNames")
           })

setMethod("folderNames", "character",
           function(x, ...) {
             folderNames(xmlParse(x), ...)
           })


setOldClass("AsIs")
setMethod("folderNames", "AsIs",
           function(x, ...) {
             folderNames(xmlParse(x), ...)
           })

tmp =  function(x, ...) {
             nodes = getNodeSet(x, "//Folder")
             structure(sapply(nodes, function(x) xmlValue(x[["name"]])),
                        names = sapply(nodes, xmlGetAttr, "id", ""))
           }

setMethod("folderNames", "KMLDoc", tmp)
setOldClass("XMLInternalDocument")
setMethod("folderNames", "XMLInternalDocument", tmp)

