
library(RKML)
load("~/Data/housing.Rda")


# marin = subset(housing, county == "Marin County")
housing = subset(housing, county == "Marin County")


ids = as.character(housing$street)


makePropDesc =
   function(row) {
      tmp = sprintf("<tr><td>%s</td><td>%s</td></tr>", names(row), row)
      paste(c("<table>", tmp, "</table>"), collapse = "\n")
   }


description =
  apply(housing[, c("city", "zip", "street", "price", "br", "lsqft", "bsqft", "year", "date")], 1,
         makePropDesc)


doc = kml(~ long + lat, housing, .names = ids, description = description)


saveXML(doc, "marin.kml", fixHTMLRefs = FALSE)


doc = kml(~ long + lat | city, housing, .names = ids, description = description)
saveXML(doc, "marinByCity.kml", fixHTMLRefs = FALSE)

