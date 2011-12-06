data(elephantSeal, package = "RKML")
library(RKML)

newData = elephantSeal
names(newData) = c("TT", "Y", "X", "days")

styles = list("ball_green" =
                 list("IconStyle" = list("scale" = "0.5",  "Icon" = c("href" = "http://www.maps.google.com/mapfiles/kml/paddle/grn-blank.png")),
                     "BalloonStyle" = "$description"),
              "ball_red" =
                 list("IconStyle" = list("scale" = "0.5",  "Icon" = c("href" = "http://www.maps.google.com/mapfiles/kml/paddle/red-blank.png")),
                       "BalloonStyle" = "$description"),
              "line_green" = list(LineStyle = list(color = I("ff8adfb2"), width = 4)),
              "line_red" = list(LineStyle = list(color = I("ff999afb"), width = 2)))

outDays = 44
direction = factor(c(rep("Out", outDays),
                     rep("Return", nrow(elephantSeal) - outDays)))

             # ~ longitude + latitude @ time, data = elephantSeal,
doc =  kml( ~ X + Y @ days, newData,
            col = c("ball_green", "ball_red")[direction], 
            lty = c("line_green", "line_red")[direction],
            docName = "Elephant Seal Travel Path",
            docDescription = "Elephant seal data <a href='http://www.stat.berkeley.edu/~brill'>Stewart & Brillinger</a>",
            docStyles = styles)


if(FALSE) {
doc =  kml( ~ X + Y @ structure(days, names = format(days, "%d-%m")), newData,
            col = c("ball_green", "ball_red")[direction], 
            lty = c("line_green", "line_red")[direction],
            docName = "Elephant Seal Travel Path",
            docDescription = "Elephant seal data <a href='http://www.stat.berkeley.edu/~brill'>Stewart & Brillinger</a>",
            docStyles = styles)           

 saveXML(doc, "sealFormula.kml")
}

if(FALSE)
o = kmlTime(elephantSeal, structure(elephantSeal$date, names = format(elephantSeal$date, "%d-%m")),
            col = c("ball_green", "ball_red")[direction],
            lty = c("line_green", "line_red")[direction], 
            docName = "Elephant Seal Travel Path",
            docDescription = "Elephant seal data <a href='http://www.stat.berkeley.edu/~brill'>Stewart & Brillinger</a>",
            docStyles = styles)
