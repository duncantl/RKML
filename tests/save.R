
if(any(w <- file.exists(c("~/foo.png", "~/foo1.png")))) {

  library(RKML)

  doc = xmlParse("extFiles.kml")
  if(!w[1])
     file.rename("~/foo1.png", "~/foo.png")    
  makeKMZArchive(doc, "/tmp/foo.kmz", force = FALSE)
  file.rename("~/foo.png", "~/foo1.png")
  if(exists("Open"))
     Open("/tmp/foo.kmz")
#  file.rename("~/foo1.png", "~/foo.png")
  
} else
 cat("No files foo.png\n")
