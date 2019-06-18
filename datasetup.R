#monthly may 15 may 16 data and yearly 15 data initially cleaned in Excel
#     removed extra rows and columns
#     added/fixed variable names
#     formatted relevant data as numbers
#cleaned up data stored with _clean in file name
dir<-getwd()
yr15<-read.csv(file = paste0(dir, "/data/", "nps_yearly_2015_clean.csv"))
may1516<-read.csv(file = paste0(dir, "/data/", "nps_monthly_may2016_clean.csv"))
#currently have only downloaded may 15 to may 16 data for monthly info
regions<-read.csv(file = paste0(dir, "/data/", "nps_regions_year_clean.csv"))
regions$RegionNameTotal=NULL
regions$ReportTotal1=NULL

parks <- read_csv(file=paste0(dir, "/data/", "parks.csv")) 
# parks$ParkName<-gsub("National Park", "NP", parks$`Park Name`)
# parks$ParkName<-gsub("NP and Preserve", "NP & PRES", parks$ParkName)

#get only the national parks
parknames<-as.character(yr15$ParkName)  
npnames<-parknames[grep("\\bNP\\b", parknames, perl = TRUE)] 

spbounddat<-get(load("data/NP_boundary_mtdt.Rda"))
spnpbound<-subset(spbounddat, spbounddat$UNIT_TYPE=="National Park")
spnamematch<-matchnames_np(as.character(spnpbound$UNIT_NAME), npnames)
XWs<-data.frame(ParkName_s=spnamematch[,2], ParkName=spnamematch[,3])
shpXW<-merge(x=XWs, y=spnpbound, by.x="ParkName_s", by.y="UNIT_NAME", all=TRUE)

prknamematch<-match_userlist(parks$`Park Name`, mdtol=0.1, rtrn="pairs") 
XWp<-data.frame(ParkName_p=prknamematch[,2], ParkName=prknamematch[,3]) 
parksXW<-merge(x=XWp, y=parks, by.x = "ParkName_p", by.y = "Park Name", all=TRUE) 
parksXWsp<-merge(x=parksXW, y=shpXW, by.x="ParkName", by.y="ParkName", all=TRUE)

colnames(parksXWsp)<-c("ParkName", "Name_parks", "Code_parks", "State_parks", "Acres", "Latitude", "Longitude", 
                       "Name_shp", "Code_shp", "GIS_Notes", "DATE_EDIT", "State_shp", "Region", "GNIS_ID", "Type_shp",
                       "Created_by", "METADATA")
save(parksXWsp, file="data/NPCrossWalk.Rda") 

yr15$isNP<-ifelse(yr15$ParkName%in%npnames, 1, 0) 
may1516$isNP<-ifelse(may1516$Park%in%npnames, 1, 0) 
regions$isNP<-ifelse(regions$ParkNameGroupTitle%in%npnames, 1, 0) 

npsubset<-function(dat) {
  newdat<-subset(dat, dat$isNP==1)
  return(newdat)
}

yr15np<-npsubset(yr15)
may1516np<-npsubset(may1516)
regionsnp<-npsubset(regions)

yrreg<-merge(x=yr15np, y=regionsnp[,1:2], by.x="ParkName", by.y = "ParkNameGroupTitle", all = TRUE)
npsdat<-merge(x=yrreg, y=may1516np[,1:7], by.x="ParkName", by.y = "Park", all = TRUE)

np<-merge(x=npsdat, y=parksXW, by.x="ParkName", by.y="ParkName", all=TRUE)

save(np, file="NPS_dat_byPark.Rda")
