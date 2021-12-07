Entropie_ligne <- function(A) {
  ENTROL <- matrix(1, nrow(spdf1@data))
  colnames(ENTROL) = "Entropie"
  rownames(ENTROL) = as.matrix(as.data.frame(spdf1@data[2]))
  
  for (l in 1: nrow(spdf1@data)){
    sommel <- sum(A[l,])
    proba_de_Mairl <- A[l,]/sommel
    vecteur_de_probal <- as.vector(proba_de_Mairl)
    probl <- vecteur_de_probal[vecteur_de_probal!=0]
    ENTROL[l] <- sum(-probl*log(probl))
    
  }
  return(ENTROL)
}