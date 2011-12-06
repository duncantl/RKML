if(FALSE) {
if(!exists("sub", globalenv())) {
if(!exists("d", globalenv())) {
   # Data as a CSV file from
   #  http://biology.st-andrews.ac.uk/seaos/documents/seaos_sg.txt
 d = read.csv("seaos_sg.txt")
 sapply(d, class)
 d$Date = as.POSIXct(strptime(as.character(d$DATE), "%d/%m/%Y %H:%M:%S"))


densityplot(~d$LAT, plot.points = FALSE, group = d$ID, auto.key = list(space = "right"))

library(maps)
#map()
map(, xlim = range(d$LON), ylim = range(d$LAT))
points(d$LON, d$LAT, col = rainbow(length(levels(d$ID)))[d$ID], pch = ".", cex = 2)
}

names(d) [4:5] = c("latitude", "longitude")

sub = do.call("rbind", by(d, d$ID, function(x) x[1:100,]))

#
# Want to average the points at the same time.
#
o = by(d, list(d$ID, d$STATION, as.POSIXlt(d$Date)$yday, as.POSIXlt(d$Date)$year), function(x) x[1,])
sub.day = do.call("rbind", o)
}


library(RKML)

styles = c("ball_green" =
                list("IconStyle" = list("scale" = "0.5",
                                        "Icon" = c("href" = "http://www.stat.berkeley.edu/users/nolan/stat133/data/greendot.png")),
                     "BalloonStyle" = "$description"),
              "ball_red" =
                 list("IconStyle" = list("scale" = "0.5",
                                         "Icon" = c("href" = "http://www.stat.berkeley.edu/users/nolan/stat133/data/reddot.png")),
                       "BalloonStyle" = "$description"))

lineIds = paste("line", letters[1:length(levels(sub$ID))], sep = "_")
styles[lineIds] = lapply(gsub("^#", "", rainbow(length(levels(sub$ID)))),
                          function(col) list(LineStyle = c(color = col, width = 4)))

library(RKML)

doc = kmlTime(sub, sub$Date, groups = sub$ID,
              col = rep("ball_green", length(levels(sub$ID))),
              lty = lineIds,
              "SEaOSd", "Southern elpehant seals as oceanographic samplers",
               styles = styles)

saveXML(doc, "seaos.kml")

}
