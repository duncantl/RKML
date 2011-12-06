library(RKML)
styles = list("ball_green" =
                 list("IconStyle" = list("scale" = "0.5",
                                         "Icon" = c("href" = system.file("Icons", "greendot.png", package = "RKML"))),
                                         "BalloonStyle" = "$description"),
              "ball_red" =
                 list("IconStyle" = list("scale" = "0.5",
                                         "Icon" = c("href" = system.file("Icons", "reddot.png", package = "RKML"))),
                                         "BalloonStyle" = "$description"),
              "line_green" = list(LineStyle = list(color = I("ff8adfb2"), width = 4)),
              "line_red" = list(LineStyle = list(color = I("ff999afb"), width = 2)))

data(elephantSeal)
outDays = 44
direction = factor(c(rep("Out", outDays),
                     rep("Return", nrow(elephantSeal) - outDays)))

desc = "Elephant seal data <a href='http://www.stat.berkeley.edu/~brill'>Stewart & Brillinger</a>"

desc = "Elephant seal data <a href='http://www.stat.berkeley.edu/~brill'>Stewart & Brillinger</a> <img src='../inst/Icons/empty.png'/>"

o = kmlTime(elephantSeal, times = elephantSeal$date, 
            name = format(elephantSeal$date, "%d-%m"),
            style = c("ball_green", "ball_red")[direction], 
            lty = c("line_green", "line_red")[direction], 
            docName = "Elephant Seal Travel Path",
            docDescription = desc,
            docStyles = styles)


#makeKMZArchive(o, "/tmp/seal.kmz")
makeKMZArchive(o, "seal.kmz", baseDir = system.file("", package = "RKML"))
