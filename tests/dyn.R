library(RKML)
dyn = createUpdatingDoc("foo.kml")
saveXML(dyn, "dyn.kml")

data(USCities)
foo = kml( ~ longitude + latitude, USCities[1:50,])
saveXML(foo, "foo.kml")

system("open dyn.kml")
