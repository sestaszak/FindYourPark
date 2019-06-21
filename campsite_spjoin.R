##spatial joins

require(ggmap)
require(rgdal)
library(rgeos)
library(sp)
require(maptools)

np<-get(load("NPS_dat_byPark.Rda"))
npcodes<-unique(np$`Park Code`)
fcamp<- read_csv("data/fed_campsites.csv")
areas<-readOGR("data/nps_boundary/nps_boundary.shp")

#nparea<-areas[areas@data$UNIT_CODE%in%npcodes ,]
nparea<-areas

#npareashpdat<-nparea@data
#save(npareashpdat, file="data/NP_boundary_mtdt.Rda")

npcamp<-unique(subset(fcamp[,c("FacilityID", "FacilityLatitude", "FacilityLongitude", "FacilityName", "AddressStateCode", "OrgAbbrevName")], fcamp$OrgAbbrevName=="NPS"))

comcrs<-CRS(proj4string(nparea))

rm(list=c("fcamp", "areas"))

npspcamp<-SpatialPointsDataFrame(npcamp[,c("FacilityLongitude", "FacilityLatitude")], data=npcamp, proj4string = comcrs)

nppcamp<-over(npspcamp, nparea)

npcampnp<-spCbind(npspcamp, nppcamp)

parkncamp<-count(npcampnp@data, UNIT_CODE)
save(parkncamp, file="data/NP_number_campg.Rda")
