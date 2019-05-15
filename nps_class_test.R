# use np dataset (from datasetup) to cluster parks

# first with kmeans, then kmeans with user_points supplied (a few parks to find similar parks to)

library(h2o)
h2o.init()

nph2o<-as.h2o(np)

predictorsnp<-c("Region", "may15.total", "may16.total", "Acres", grep("YTD", names(np), value=TRUE))

kmod<-h2o.kmeans(nph2o, predictorsnp, k=10, estimate_k = TRUE)
kmod
h2o.centers(kmod)

#upcenters<-

pcamod<-h2o.prcomp(nph2o, predictorsnp, transform="NONE", k=1, impute_missing = TRUE)
pcamod
