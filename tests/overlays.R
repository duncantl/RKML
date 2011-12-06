# This example shows how to put R plots at different locations

if(FALSE) {
 delays2008 = read.csv("~/Data/Airline/2008.csv")
 ids = names(sort(table(delays2008$Origin), dec = TRUE)[1:20])
 airDelays = subset(delays2008, Origin %in% ids)

#load("BigAirportDelays2008.rda") 
}


