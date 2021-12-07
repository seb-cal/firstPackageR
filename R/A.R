A <- function(mot,spdf) {
  
  i<-which(colnames(DATA)==mot) 
  spdf1<-lspdf[[i]]
  spdf.geo<- spdf
  A<-inter_spdf(spdf1,2,spdf.geo,2)
  return(A$A)
  
}