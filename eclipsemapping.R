campsites<-read.csv("data/fed_campsites.csv")
orcamp<-subset(campsites, campsites$AddressStateCode=="OR")
orcampun<-unique(orcamp[,c("FacilityLatitude", "FacilityLongitude", "FacilityName", "AddressStateCode", "OrgAbbrevName")])

currentairbnb<-data.frame(lat=44.04728, long=-123.10314199999999, place="Downtown Villa")

require(ggmap)
require(rgdal)
require(maptools)

testormap2<-get_map(location="oregon", zoom=7.25, color="bw")
testormap2<-get_map(location = c(lon=-122.1 , lat=45), zoom=8, color="bw")
area <- readShapePoly("data/eclips2017_1/upath17_1s.shp")
area.points<-fortify(area)

areac <-readShapeLines("data/eclips2017_1/ucenter17_1s.shp")
areac.points<-fortify(areac)

ggmap(testormap2) + geom_polygon(aes(x=long, y=lat, group=group), data=area.points, alpha=0.5) + labs(x="Longitude", y="Latitude")

gline<-ggmap(testormap2) + geom_path(aes(x=long, y=lat), data=area.points, alpha=0.5) + geom_path(aes(x=long, y=lat), data=areac.points, alpha=.5, col="red")

gline + labs(x="Longitude", y="Latitude") + geom_point(aes(x=long , y=lat, color=place), data=currentairbnb)

#+ geom_point(aes(x=FacilityLongitude, y=FacilityLatitude, color=OrgAbbrevName), data=orcampun)


## export to kml for google maps
area<-readOGR(dsn="data/eclips2017_1", layer="upath17_1s")
areac<-readOGR(dsn="data/eclips2017_1", layer="ucenter17_1s")


b<-bbox(area)
b[1,]<-c(-125, -120)
b[2,]<-c(43, 46)


areasub<-area
apoly<-area@polygons
apolycoords<-apoly[[1]]@Polygons[[1]]@coords
newpolycoords<-subset(apolycoords, apolycoords[,1]<=-120 & apolycoords[,1]>=-126)
newapoly<-apoly
newapoly[[1]]@Polygons[[1]]@coords<-newpolycoords
areasub@polygons<-newapoly
areaWGS<-spTransform(areasub, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
writeOGR(areaWGS, dsn="data/eclipse_outerWGS.kml", layer="eclipse_outerWGS", driver="KML", overwrite_layer = TRUE)

linesub<-areac
clines<-areac@lines[[1]]@Lines[[1]]@coords
newlinecoord<-subset(clines, clines[,1]<=-120 & clines[,1]>=-126)
linesub@lines[[1]]@Lines[[1]]@coords<-newlinecoord

areacWGS<-spTransform(linesub, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
writeOGR(areacWGS, dsn="data/eclipse_centerWGS.kml", layer="eclipse_centerWGS", driver="KML", overwrite_layer = TRUE)

cacamp<-subset(campsites, campsites$AddressStateCode=="CA")
cacampun<-unique(cacamp[,c("FacilityLatitude", "FacilityLongitude", "FacilityName", "AddressStateCode", "OrgAbbrevName")])
camap<-get_map(location="california")
ggmap(camap)

ggmap(camap) + geom_point(aes(x=FacilityLongitude, y=FacilityLatitude, color=OrgAbbrevName), data=cacampun)

trinityarea<-subset(cacampun, cacampun$FacilityLatitude<40.8 & cacampun$FacilityLatitude>40.5 & cacampun$FacilityLongitude > -123.0 & cacampun$FacilityLongitude < -122.2)
