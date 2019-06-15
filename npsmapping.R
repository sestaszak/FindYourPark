require(ggmap)
require(rgdal)
library(rgeos)
library(sp)
require(maptools)

np<-get(load("NPS_dat_byPark.Rda"))
npcodes<-unique(np$`Park Code`)
# area <- readShapePoly("data/nps_boundary/nps_boundary.shp")
### use rgdal::readOGR or sf::st_read
#area<-readOGR("data/National_Park_Service__Park_Unit_Boundaries/National_Park_Service__Park_Unit_Boundaries.shp")
area<-readOGR("data/National_Park_Service__Park_Unit_Centroids/National_Park_Service__Park_Unit_Centroids.shp")

centnps<-area[area@data$UNIT_CODE%in%npcodes,]

#area.points<-fortify(area)

### google maps doesn't work and location not as bbox calls google maps
#mapusa<-get_map(location="united states", zoom=3, source = "stamen", maptype="toner")
usbbox<-c(left = -125.76 , bottom = 18.44, right= -63.19, top=50.598)
mapusa<-get_stamenmap(bbox=usbbox, zoom=3, maptype = "toner")

##runs slowly on macbook:
#ggmap(mapusa) + geom_polygon(aes(x=long, y=lat, group=group), data=area.points, alpha=0.5)

#plot_usmap(regions="states") + geom_polygon(aes(x=long, y=lat, group=group, color="green"), data=area.points)


###flag campsites/campgrounds by park
campsites<-read.csv("data/fed_campsites.csv")
campsites<-campsites[-which(is.na(campsites$FacilityLongitude)),]
campsites$ID<-paste0(campsites$FacilityID, "_", campsites$CampsiteID)

campsiteLL<-data.frame(Longitude=campsites$FacilityLongitude, Latitude=campsites$FacilityLatitude, ID=campsites$ID)

coordinates(campsiteLL) <- ~ Longitude + Latitude
# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(campsiteLL) <- proj4string(area)

campinpark<-sp::over(area, campsiteLL) ## returns set of campsite ids i want, but nothing else
parkcamp<-sp::over(campsiteLL, area) ## returns a park name for all campsites but no campsite info

campflag<-data.frame(campsites, ParkCode=parkcamp$UNIT_CODE)

npcodes<-unique(np$`Park Code`)

npcampfl<-subset(campflag, campflag$ParkCode%in%npcodes)
