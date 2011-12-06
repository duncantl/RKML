#source("~/Books/XMLTechnologies/Rpackages/RKML/R/gePlugin.R")
library(RKML)

base = "http://www.omegahat.org/RKML/JSM2011/imageAnimation"
docs = c(ge.timeslice = "timeslice_current.kml",
           ge.RCM = "gcmrcm_current.kml")
kdocs = structure(sprintf("%s/%s", base, docs),       names = names(docs))

doc = setAPIKey("xxx")
tt = copyPluginJSCode(NA, kdocs, TRUE, "~/Books/XMLTechnologies/Rpackages/RKML/inst/pluginTemplate/geMultiInitialization.js")
ndoc = insertJS(doc, I(tt), replace = TRUE)
ndoc
