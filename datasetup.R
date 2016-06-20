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

#get only the national parks
parknames<-as.character(yr15$ParkName)
npnames<-parknames[grep("\\bNP\\b", parknames, perl = TRUE)]

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
npsdat<-merge(x=yrreg, y=may1516np[,1:7], by.x = "ParkName", by.y = "Park", all = TRUE)
