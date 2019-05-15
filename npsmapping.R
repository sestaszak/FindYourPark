require(ggmap)
require(rgdal)
library(rgeos)
library(sp)
require(maptools)

# area <- readShapePoly("data/nps_boundary/nps_boundary.shp")
### use rgdal::readOGR or sf::st_read
area<-readOGR("data/nps_boundary/nps_boundary.shp")

area.points<-fortify(area)


mapusa<-get_map(location="united states", zoom=3)

ggmap(mapusa) + geom_polygon(aes(x=long, y=lat, group=group), data=area.points, alpha=0.5)

plot_usmap(regions="states") + geom_polygon(aes(x=long, y=lat, group=group, color="green"), data=area.points)


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
