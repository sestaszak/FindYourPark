library(stringdist)
### source: http://bigdata-doctor.com/fuzzy-string-matching-survival-skill-tackle-unstructured-information-r/

npdatid<-npnames #source2
#pdatid<-parks$`Park Name` #source1
pdatid<-parks$ParkName

distance.methods<-c('osa','lv','dl','lcs','qgram','cosine','jaccard','jw')
dist.methods<-list()
for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(npdatid),nrow = length(pdatid))
  for(i in 1:length(npdatid)) {
    for(j in 1:length(pdatid)) { 
      dist.name.enh[j,i]<-stringdist(tolower(npdatid[i]),tolower(pdatid[j]),method = distance.methods[m])      
      #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
}

#### just a test for seki only
nptrouble<-"Sequoia and Kings Canyon National Parks"
dist.name.enh<-matrix(NA, ncol=length(distance.methods), nrow=length(npdatid))
row.names(dist.name.enh)<-npdatid
colnames(dist.name.enh)<-distance.methods
for(m in 1:length(distance.methods)){
  for(i in 1:length(npdatid)){
    dist.name.enh[i, m]<-stringdist(tolower(npdatid[i]), tolower(nptrouble), method=distance.methods[m])
  }
}

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods))
{
  
  dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh<-apply(dist.matrix, 1, base::min)
  for(i in 1:nrow(dist.matrix))
  {
    s2.i<-match(min.name.enh[i],dist.matrix[i,])
    s1.i<-i
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name= npdatid[s2.i], #source2.devices[s2.i,]$name, 
                                                          s1name= pdatid[i], #source1.devices[s1.i,]$name, 
                                      adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}
# Let's have a look at the results
# library(reshape2)
matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix)
