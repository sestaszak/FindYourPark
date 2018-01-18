#read in a vector of character strings of parks visited
#output dataset with vector added 1 for visited, 0 for not
agrep_vec_sapply<-function(vec, mdtol, lookup) {
  agm<-sapply(vec, FUN=function(x) agrep(x, lookup, max.distance = mdtol), simplify = TRUE)
  agml<-sapply(agm, length)
  md<-unlist(agm[which(agml==1)])
  fx<-which(agml!=1)
  
  match_agrep<-list(matched=md, tofix=fx)
  return(match_agrep)
}

match_userlist<-function(visit_vector, mdtol, rtrn) {
  trymatch1<-agrep_vec_sapply(vec=visit_vector, mdtol=mdtol, lookup = npnames)
  matched<-trymatch1$matched
  tofix<-trymatch1$tofix
  pairmatch<-cbind(matched, visit_vector[which(!visit_vector%in%visit_vector[tofix])])
  if (length(matched) != length(visit_vector)) {
    trynp<-paste(visit_vector[tofix], "NP", sep=" ")
    trymatch2<-agrep_vec_sapply(vec=trynp, mdtol=mdtol, lookup = npnames)
    match2<-trymatch2$matched
    pairm2<-cbind(match2, visit_vector[which(!visit_vector%in%visit_vector[trymatch2$tofix])])
    trynpsub<-gsub("National Park", "NP", visit_vector)
    trynpsub<-gsub("NP and Preserve", "NP & PRES", trynpsub)
    trymatch3<-agrep_vec_sapply(vec=trynpsub, mdtol=mdtol, lookup = npnames)
    match3<-trymatch3$matched
    pairm3<-cbind(match3, visit_vector[which(!visit_vector%in%visit_vector[trymatch3$tofix])])
    if (length(match2)==length(tofix)) {
      allmatch<-c(matched, match2)
      allpair<-rbind(pairmatch, pairm2)
    }
    else if (length(match3)==length(tofix)) {
      allmatch<-c(matched, match3)
      allpair<-rbind(pairmatch, pairm3)
    }
    else {
      if(length(match3)>length(match2)){newmatched<-trymatch3}
      else {newmatched<-trymatch2}
      newtofix<-newmatched$tofix
      newpairm<-cbind(newmatched$matched, visit_vector[which(!visit_vector%in%visit_vector[newtofix])])
      matchsofar<-c(matched, newmatched$matched)
      pairsofar<-rbind(pairmatch, newpairm)
      for(i in 1:length(newtofix)){
        if(grepl("and", visit_vector[newtofix[i]])){
          tosplit<-gsub("National Park", "", visit_vector[newtofix[i]])
          splitlst<-strsplit(tosplit, "and")
          splitfix<-do.call(c, splitlst)
          trymatch4<-agrep_vec_sapply(vec=splitfix, mdtol=mdtol, lookup = npnames)
          matchsofar<-c(matchsofar, trymatch4$matched)
          pairi<-cbind(trymatch4$matched, rep(visit_vector[newtofix[i]], length(trymatch4$matched)))
          pairsofar<-rbind(pairsofar, pairi)
        } 
        else {print("No and in name, not sure what to try")}
      }
      if(length(unique(matchsofar))>=length(visit_vector)) {
        allmatch<-matchsofar
        allpair<-pairsofar
        }
      else {print("Something went wrong here length not correct")}
    }
    
  }
  else {allmatch<-matched}
  
  if(rtrn=="flag") {
  visitflag<-rep(0, length(npnames))
  visitflag[allmatch]<-1
  return(visitflag)
  }
  else if(rtrn=="strings") {
    return(npnames[allmatch])
  }
  else if(rtrn=="pairs") {
    allpairname<-cbind(allpair, npnames[as.numeric(allpair[,1])])
    return(allpairname)
  }
}