lapply(sprintf("/tmp/plot%d.png", 1:20),  function(i) { png(i); plot(1:10); dev.off()})

library(RKML)
data(quakes)
desc = sprintf("<img src='%s'>", sprintf("/tmp/plot%d.png", 1:20))
desc = rep(desc, length = nrow(quakes))

doc = kmlPoints(quakes, description = desc)
saveXML(doc, "/tmp/test.kml")
