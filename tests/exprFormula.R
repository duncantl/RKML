library(RKML)
data(USCities)

kml( ~ - longitude + latitude, USCities)

#rownames(USCities) = USCities$name
USCities$day = sample(1:5, nrow(USCities), replace = TRUE)

cities = c("Seattle", "SanFrancisco", "LosAngeles", "Atlanta", "Chicago", "Boston", "MiamiIntl", "Portland", "Dallas/FW", "MemphisIntl")

tmp = subset(USCities, name %in% cities)
o = kml( ~ - longitude + latitude @ day, tmp, addLines = FALSE, description = tmp$name)
saveXML(o, "cities.kml")


# Note kml( ~ - longitude + latitude @ 2 * day, subset(USCities, name %in% cities), addLines = FALSE)
# doesn't parse as the expression after the @ is not "atomic"
o = kml( ~ - longitude + latitude @  I(2 * day), subset(USCities, name %in% cities), addLines = FALSE)

