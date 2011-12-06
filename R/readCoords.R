setGeneric("getCoordinates",
           function(x, ...)
               standardGeneric("getCoordinates"))

setMethod("getCoordinates", "character",
           function(x, ...) {
             getCoordinates(xmlParse(x))
           })

setOldClass("AsIs")
setMethod("getCoordinates", "AsIs",
           function(x, ...) {
              doc = xmlParse(x, asText = TRUE)
              getCoordinates(doc)
           })

setMethod("getCoordinates", "XMLInternalDocument",
           function(x, ...) {
              xpathApply(x, "//x:coordinates", getCoordinates, namespaces = "x")
           })

setMethod("getCoordinates", "XMLInternalElementNode",
           function(x, ...) {
              dd = read.csv(textConnection(xmlValue(x)), header = FALSE)
              dd[ !is.na(dd[,1]), ]
           })
