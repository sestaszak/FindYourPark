##species dataset
library(tidyverse)
library(readxl)
species <- read_excel("data/species.xlsx")
species$X__1<-NULL
# species<-read.csv(paste0(dir, "/data/", "species.csv"), fill=TRUE)

speciesok<-species[,1:5]
speciesfix<-species[,c(1, 6:ncol(species))]

table(speciesfix$`Record Status`)
#acceptable status: Approved, In Review
statusok<-c("Approved", "In Review")


csvprob<-speciesfix[which(!speciesfix$`Record Status`%in%statusok),]
csvprob1<-csvprob[which(csvprob$`Record Status`=="None"),]
#common name should be "None" shift over
csvprob1$`Scientific Name`<-paste(csvprob1$`Scientific Name`, csvprob1$`Common Names`, sep=" ")
csvprob1$`Common Names`<-NULL
colnames(csvprob1)<-c(names(csvprob1)[1:2], "Common Names", "Record Status", "Occurrence", "Nativeness", "Abundance", "Seasonality")
csvprob1$`Conservation Status`<-rep(NA, nrow(csvprob1))

csvprob2<-csvprob[which(!csvprob$`Species ID`%in%csvprob1$`Species ID`),]
#record status shouldn't be common name
csvprob2$`Common Names`<-paste(csvprob2$`Common Names`, csvprob2$`Record Status`, sep=", ")
csvprob2$`Record Status`<-NULL
colnames(csvprob2)<-c(names(csvprob2)[1:3], "Record Status", "Occurrence", "Nativeness", "Abundance", "Seasonality")
csvprob2$`Conservation Status`<-rep(NA, nrow(csvprob2))

csvfixed<-rbind(csvprob1, csvprob2)
speciesfix[which(speciesfix$`Species ID`%in%csvfixed$`Species ID`), ]<-csvfixed

speciesall<-cbind(speciesok, speciesfix)


##species
## group by park name and create variables that are counts of: 
#   -num catetory
#   -num species (scientific name)
#   -num with !is.na(conservation status)
#   -

#ok descriptives now that csv issue fixed
table(speciesall$`Record Status`)
#ok

table(speciesall$Occurrence)
#present only

table(speciesall$Nativeness)
table(speciesall$Abundance)
table(speciesall$Seasonality)
#multiple options, would need regex

table(speciesall$`Conservation Status`)
#just count !is.na

pspecies<-subset(speciesall, speciesall$Occurrence=="Present")

parktot<-count(pspecies, `Park Name`)

parktype<-count(pspecies, `Park Name`, Category)
parknat<-count(pspecies, `Park Name`, Nativeness)
parkab<-count(pspecies, `Park Name`, Abundance)














