Caffiche <- function(Mots,Type_de_clust,Nb_groupe,parti) {
  i<-which(colnames(DATA)==Mots) 
  spdf1<-lspdf[[i]]
  affichage_clust(mot = Mots,nom_de_la_fonction = Type_de_clust,spdf.geo = parti)
  test <- cutree(monClust,Nb_groupe)
  map_clust(cut = test,spdf1 = spdf1)
}