Calcule_statistique_base <- function(mot,spdf.geo) {
  i<-which(colnames(DATA)==mot)
  notion<-mot
  spdf1<-lspdf[[i]]
  
  if (length(spdf1)==1) {
    return(list(notion=notion, khi_deux = 0, Phi_deux = 0, V2_de_Cramer = 0, Total_indice_spe = 1, Indice_de_spe = 1, Indice_de_localisation = 1, entropie = 0 ))
  } else{
    
    # Matrice des superpositions A
    A<-inter_spdf(spdf1,2,spdf.geo,2)$A ; A
    # Creation d'une matrice theorique dess  khi-deux
    Ath <- chisq.test(A)$expected
    Ack <- (A-Ath)^2 / Ath
    
    
    Khi <- sum(Ack)
    Phi <- Khi / sum(A)
    VCram <- Phi / min(nrow(spdf1@data)-1 ,5)
    
    
    
    mot <- nrow(spdf1@data)
    SPE <- matrix(1,mot)
    colnames(SPE) = "Indice de Spe"
    rownames(SPE) = as.matrix(as.data.frame(spdf1@data[2]))
    for (i in 1:mot){
      SPE[i] <- 0.5 * sum(abs(A[i,] - Ath[i,])) / sum(A[i,]) }
    TOTAL <- raster::weighted.mean(x = SPE[,1],w = margin.table(A,1)/sum(A))
    sp <- rbind(SPE,TOTAL)
    
    
    
    region <- nrow(spdf.geo@data)
    LOC <- matrix(1,region)
    colnames(LOC) = "Indice de Loc"
    rownames(LOC) = as.matrix(as.data.frame(spdf.geo@data[2]))
    for (j in 1:region){
      LOC[j] <- 0.5 * sum(abs(A[,j] - Ath[,j])) / sum(A[,j]) }
    lo <- rbind(LOC,TOTAL)
    
    somme <- sum(A)
    proba_de_Mair <- A/somme
    
    vecteur_de_proba <- as.vector(proba_de_Mair)
    prob <- vecteur_de_proba[vecteur_de_proba!=0]
    entropiex <- sum(-prob*log(prob))
    
    ENTROL <- matrix(1,mot)
    colnames(ENTROL) = "Entropie"
    rownames(ENTROL) = as.matrix(as.data.frame(spdf1@data[2]))
    
    for (l in 1:mot){
      sommel <- sum(A[l,])
      proba_de_Mairl <- A[l,]/sommel
      vecteur_de_probal <- as.vector(proba_de_Mairl)
      probl <- vecteur_de_probal[vecteur_de_probal!=0]
      ENTROL[l] <- sum(-probl*log(probl))
      
    }
    
    #Les sorties sont : Khi, Phi, Vcram, TOTAL, sp, lo
    
    return(list(notion=notion, khi_deux = Khi, Phi_deux = Phi, V2_de_Cramer = VCram, Total_indice_spe = TOTAL, Indice_de_spe = sp, Indice_de_localisation = lo, entropie = entropiex, entropie_ligne = ENTROL ))
  }
}