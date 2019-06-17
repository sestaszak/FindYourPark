## imputation

# 0 imputation
zeroimpute<-function(cl){
  cl<-ifelse(is.na(cl), 0, cl)
}

# median imputation
medianimpute<-function(cl){
  mdr<-median(cl, na.rm = TRUE)
  cl<-ifelse(is.na(cl), mdr, cl)
}