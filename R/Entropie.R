Entropie <- function(A) {
  #A est le tableau de contingence des aires 
  somme <- sum(A)
  proba_de_Mair <- A/somme
  
  vecteur_de_proba <- as.vector(proba_de_Mair)
  prob <- vecteur_de_proba[vecteur_de_proba!=0]
  entropiex <- sum(-prob*log(prob))
  return(entropiex)
}