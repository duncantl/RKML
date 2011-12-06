setGeneric("HTMLTable",
            function(x, labels = names(x), byRow = TRUE, ...)
               standardGeneric("HTMLTable"))

makeRow =
function(x, parent = NULL, type = "th")
  newXMLNode("tr", parent = parent,
                  .children = lapply(x, function(x) newXMLNode(type, x)))

setMethod("HTMLTable", "vector",
           function(x, labels = names(x), byRow = TRUE, ...) {
             if(byRow == FALSE) {

               rows = list(if(length(labels)) makeRow(labels),
                            makeRow(x, type = 'td'))
               
             } else {
             
               rows = if(length(labels))
                         mapply(function(x, id) {
                                 newXMLNode("tr", newXMLNode("td", id), newXMLNode("td", x))
                                },  x, labels)
                    else
                       sapply(x, function(x) newXMLNode("tr",  newXMLNode("td", x)))
             }

             newXMLNode("table", .children = rows)
           })

setMethod("HTMLTable", "data.frame",
           function(x, labels = names(x), byRow = TRUE, ...) {
             tb = newXMLNode("table")
              if(length(labels))
                  newXMLNode("tr", parent = tb,
                              .children = lapply(labels, function(x) newXMLNode("th", x)))
             
              for(i in seq_len(nrow(x))) {
                 row = newXMLNode("tr", parent = tb)
                 for(j in seq_len(ncol(x)))
                   newXMLNode("td", x[i, j], parent = row)
             }
             tb
           })

