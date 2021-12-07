charger_Donnees <- function(cheminDonnees) {
  # Consigne d'utilisation : Telecharger le dossier Donnees,
  # et donner le chemin d'access vers ce dossier dans cheminDonnees
  
  nomFichier<-c("/thesoc_spdf.RData","/thesoc_data.RData","/spdfGEO.Rdata","/spdfDPT.Rdata")
  
  chemin<-rep(cheminDonnees,length(nomFichier))
  for (i in 1:length(chemin)) {
    chemin[i]<-paste(chemin[i],nomFichier[i],sep = "")
  }
  
  lapply(chemin,load,.GlobalEnv)
  
}