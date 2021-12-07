affichage_clust <- function(nom_de_la_fonction,mot,spdf.geo) {
  
  i<-which(colnames(DATA)==mot) 
  spdf1<-lspdf[[i]]
  A<-inter_spdf(spdf1,2,spdf.geo,2)$A
  
  a <- nom_de_la_fonction(mot,1,spdf.geo)
  i<-which(colnames(DATA)== mot)
  spdf1<-lspdf[[i]]
  A<-inter_spdf(spdf1,2,spdf.geo,2)$A
  
  #Cr?ation d'un clust :
  monClust<<-hclust(dist(A))
  monClust$merge<<-a$merge
  monClust$height<<- abs(a$height)
  monClust$order<<- a$order
  monClust$labels<<-a$labels
  
  #D?ciner le clust : 
  plot(monClust)
  
}