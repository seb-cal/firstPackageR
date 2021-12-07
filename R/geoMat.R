A <- function(mot,spdf) {
  
  i<-which(colnames(DATA)==mot) 
  spdf1<-lspdf[[i]]
  spdf.geo<- spdf
  A<-inter_spdf(spdf1,2,spdf.geo,2)
  return(A$A)
  
}

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

affichage<-function(mot, partition){
  if (!is.numeric(mot)){
    mot<-which(colnames(DATA)==mot)
  }
  spdf1<-lspdf[[mot]]
  
  #modification d'echelle pour les notions avec un grand nombre de lemmes
  if (nlevels(spdf1$lemme)>10) {
    coeflegend<-0.55
    plotxlim<-c(-2,15)
  }else{
    coeflegend<-0.9
    plotxlim<-c(-1.656071,7.67589)
  }
  
  
  palette<-rainbow(nrow(spdf1@data))
  plot(spdf1,col=palette, main=paste('cartographie de la notion',colnames(DATA)[mot]),xlim=c(-2,13.4))
  plot(partition,lwd=2,add=TRUE)
  legend("topright",legend=spdf1$lemme,fill=palette,ncol=3,cex = coeflegend)
}

Caffiche <- function(Mots,Type_de_clust,Nb_groupe,parti) {
  i<-which(colnames(DATA)==Mots) 
  spdf1<-lspdf[[i]]
  affichage_clust(mot = Mots,nom_de_la_fonction = Type_de_clust,spdf.geo = parti)
  test <- cutree(monClust,Nb_groupe)
  map_clust(cut = test,spdf1 = spdf1)
}

calcul_ordre_dendro<-function(merge,n){
  A<-rev(as.vector(t(merge)))
  p<-length(A)
  idpos<-which(A>0)
  
  while(length(idpos)>0 & idpos[1]<=n){
    i<-which(A>0)[1]
    if(i==1) A<-c(merge[A[1],],A[-1])
    if(i> 1) A<-c( A[1:(i-1)]  ,  merge[A[i],]  ,  A[(i+1):p])
    p<-length(A)
    idpos<-which(A>0)
  }
  return(-A[1:n])
}

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

charger_Donnees <- function(cheminDonnees) {
  # Consigne d'utilisation : Telecharger le dossier data,
  # et donner le chemin d'access vers ce dossier dans cheminDonnees
  
  nomFichier<-c("/thesoc_spdf.RData","/thesoc_data.RData","/spdfGEO.Rdata","/spdfDPT.Rdata")
  
  chemin<-rep(cheminDonnees,length(nomFichier))
  for (i in 1:length(chemin)) {
    chemin[i]<-paste(chemin[i],nomFichier[i],sep = "")
  }
  
  lapply(chemin,load,.GlobalEnv)
  
}

clust_Entropie_geo <- function(mots,nbgroupe,spdf_geo) {
  
  i<-which(colnames(DATA)== mots)
  spdf1<-lspdf[[i]]
  A<-inter_spdf(spdf1,2,spdf_geo,2)$A
  A <- t(A)
  dep <- A
  khi_b <- chisq.test(dep)$statistic
  # Renommage de la matrice A
  Nb_mots <- length(A[,1])
  coucou <- Nb_mots
  Nb_region <- length(A[1,])
  nom_col <- as.vector(seq(-1,-Nb_region,-1))
  nom_row <- as.vector(seq(-1,-Nb_mots,-1))
  colnames(A) <- nom_col
  rownames(A) <- nom_row
  
  # Nombres de tour
  combien <- Nb_mots - nbgroupe
  
  #Variable utile pour la suite
  valeur_de_entropie <- c()
  Matrice_de_cont <- c()
  nom_de_la_variable_cree <- 1
  nb_combi <- (Nb_mots*(Nb_mots+1)/2)-Nb_mots
  historique <- matrix(NA,nrow = nb_combi,ncol = 2)
  Histo_final <- matrix(NA,nrow = combien,ncol = 2)
  possition <- 1
  test <- 1
  labels <- rownames(dep)
  hii<-c()
  
  #debut la boucle
  
  #Boucle qui calcule toute les unions possible et par la suite calcule le nouveau khi-deux
  
  
  #Boucle pour savoir le nombre de regroupement que l'on veut faire
  for(nb in 1:combien) {
    
    historique <- matrix(NA,((Nb_mots*(Nb_mots+1)/2)-Nb_mots),2)
    #Savoir le nombre de mots et le mots et le nombre de regions
    Nb_mots <- length(A[,1])
    
    #Boucle pour selèctionner l'ensemble des paires  calculer
    for (i in 1:Nb_mots) {
      for (p in 1:Nb_mots){
        if (i == p) {
          NULL
        }
        else if (p < i){
          NULL
        }
        else {
          
          # Historique de touts les paires de regroupement
          
          nom_des_row <- rownames(A)
          nom_des_col <- rownames(A)
          historique[test,1] <- nom_des_row[i]
          historique[test,2] <- nom_des_col[p]
          test <- test +1
          
          # Addition des deux lignes choisie 
          cumule <- A[i,] + A[p,]
          
          #Suppresion des deux  lignes ajoutees dans l'etapes precedante
          nom <- rownames(A)
          nom <- nom[c(-i,-p)]
          Abis <- (A[c(-i,-p),]) 
          
          
          
          #Creation de la nouvelle matrice avec le cumule des deux lignes i et j
          Nouveau_matrice_de_cont <- (rbind(cumule,Abis))
          
          # Changement du nom de lignes
          rownames(Nouveau_matrice_de_cont)[rownames(Nouveau_matrice_de_cont)=="cumule"] <- as.character(possition)  
          
          #Cas de l'avant dernier  groupe 
          rownames(Nouveau_matrice_de_cont)[rownames(Nouveau_matrice_de_cont)=="Abis"] <- as.character(nom)
          
          rm(nom)
          #Calcule de l'entropie sur la matrice de la ligne d'avant
          
          Entropie_bis <- Entropie(Nouveau_matrice_de_cont)
          valeur_de_entropie <- rbind(valeur_de_entropie,Entropie_bis)  
          
          
          #Recuperation des matrices de contingence de toutes les combinaison
          Matrice_de_cont <- rbind(Matrice_de_cont,Nouveau_matrice_de_cont)
        }
      }
    } 
    
    #Traitement des Khi-deux
    valeur_de_entropie <- as.vector(valeur_de_entropie)
    possition_du_min <- which.min(valeur_de_entropie)
    hii <- c(hii,(khi_b-valeur_de_entropie[possition_du_min]))
    ligne_de_debut <- (Nb_mots-1) * possition_du_min
    
    #Recuperation du nouveau tableau de contingence
    A <- Matrice_de_cont[c(((ligne_de_debut+1)- (length(A[,1])-1) ):(ligne_de_debut)),]
    
    #gestion des historiques
    valeurs <- historique[possition_du_min,]
    Histo_final[nb,] <- as.numeric(valeurs)
    possition <- possition+1
    
    #Renisialisation des variables
    test <- 1
    valeur_de_entropie <- c()
    possition_du_min <- c()
    valeur_de_entropie <- c()
    ligne_de_debut <- c()
    Matrice_de_cont <- c()
    nom_des_row <- c()
    nom_des_col <- c()
    historique <- c()
    
    #Fin de la boucle
  }
  
  #Fin
  
  if (class(A) == "numeric") {
    A <- matrix(A,nrow = 1)
    rownames(A) <- coucou
  }
  
  
  # Ordre du dendrogramme :
  Depart2 <- calcul_ordre_dendro(n = coucou,merge = Histo_final)
  
  
  
  
  # Sortie pour la fonction :
  return(list(tableau = A, merge = Histo_final, labels = labels, order = Depart2,height = sort(abs(hii),decreasing = FALSE ) ))
}

clust_Entropie_notion <- function(mots,nbgroupe,spdf_geo) {
  
  i<-which(colnames(DATA)== mots)
  spdf1<-lspdf[[i]]
  A<-inter_spdf(spdf1,2,spdf_geo,2)$A
  dep <- A
  khi_b <- chisq.test(dep)$statistic
  # Renommage de la matrice A
  Nb_mots <- length(A[,1])
  coucou <- Nb_mots
  Nb_region <- length(A[1,])
  nom_col <- as.vector(seq(-1,-Nb_region,-1))
  nom_row <- as.vector(seq(-1,-Nb_mots,-1))
  colnames(A) <- nom_col
  rownames(A) <- nom_row
  
  #Nombres de tour
  combien <- Nb_mots - nbgroupe
  
  #Variable utile pour la suite
  valeur_de_entropie <- c()
  Matrice_de_cont <- c()
  nom_de_la_variable_cree <- 1
  nb_combi <- (Nb_mots*(Nb_mots+1)/2)-Nb_mots
  historique <- matrix(NA,nrow = nb_combi,ncol = 2)
  Histo_final <- matrix(NA,nrow = combien,ncol = 2)
  possition <- 1
  test <- 1
  labels <- rownames(dep)
  hii<-c()
  
  #debut la boucle
  
  #Boucle qui calcule toute les unions possible et par la suite calcule le nouveau khi-deux
  
  
  #Boucle pour savoir le nombre de regroupement que l'on veut faire
  for(nb in 1:combien) {
    
    historique <- matrix(NA,((Nb_mots*(Nb_mots+1)/2)-Nb_mots),2)
    #Savoir le nombre de mots et le mots et le nombre de regions
    Nb_mots <- length(A[,1])
    
    #Boucle pour selèctionner l'ensemble des paires  calculer
    for (i in 1:Nb_mots) {
      for (p in 1:Nb_mots){
        if (i == p) {
          NULL
        }
        else if (p < i){
          NULL
        }
        else {
          
          # Historique de touts les paires de regroupement
          
          nom_des_row <- rownames(A)
          nom_des_col <- rownames(A)
          historique[test,1] <- nom_des_row[i]
          historique[test,2] <- nom_des_col[p]
          test <- test +1
          
          # Addition des deux lignes choisie 
          cumule <- A[i,] + A[p,]
          
          #Suppresion des deux  lignes ajoutees dans l'etapes precedante
          nom <- rownames(A)
          nom <- nom[c(-i,-p)]
          Abis <- (A[c(-i,-p),]) 
          
          
          
          #Creation de la nouvelle matrice avec le cumule des deux lignes i et j
          Nouveau_matrice_de_cont <- (rbind(cumule,Abis))
          
          # Changement du nom de lignes
          rownames(Nouveau_matrice_de_cont)[rownames(Nouveau_matrice_de_cont)=="cumule"] <- as.character(possition)  
          
          #Cas de l'avant dernier  groupe 
          rownames(Nouveau_matrice_de_cont)[rownames(Nouveau_matrice_de_cont)=="Abis"] <- as.character(nom)
          
          rm(nom)
          #Calcule de l'entropie sur la matrice de la ligne d'avant
          
          Entropie_bis <- Entropie(Nouveau_matrice_de_cont)
          valeur_de_entropie <- rbind(valeur_de_entropie,Entropie_bis)  
          
          
          #Recuperation des matrices de contingence de toutes les combinaison
          Matrice_de_cont <- rbind(Matrice_de_cont,Nouveau_matrice_de_cont)
        }
      }
    } 
    
    #Traitement des Khi-deux length(A[,1])
    valeur_de_entropie <- as.vector(valeur_de_entropie)
    possition_du_min <- which.min(valeur_de_entropie)
    hii <- c(hii,(khi_b-valeur_de_entropie[possition_du_min]))
    ligne_de_debut <- ((length(A[,1]))-1) * possition_du_min
    
    #Recuperation du nouveau tableau de contingence
    A <- Matrice_de_cont[c(((ligne_de_debut+1)- (length(A[,1])-1) ):(ligne_de_debut)),]
    
    #gestion des historiques
    valeurs <- historique[possition_du_min,]
    Histo_final[nb,] <- as.numeric(valeurs)
    possition <- possition+1
    
    #Renisialisation des variables
    test <- 1
    valeur_de_entropie <- c()
    possition_du_min <- c()
    valeur_de_entropie <- c()
    ligne_de_debut <- c()
    Matrice_de_cont <- c()
    nom_des_row <- c()
    nom_des_col <- c()
    historique <- c()
    
    #Fin de la boucle
  }
  
  #Fin
  
  if (class(A) == "numeric") {
    A <- matrix(A,nrow = 1)
    rownames(A) <- coucou
  }
  
  
  # Ordre du dendrogramme :
  Depart2 <- calcul_ordre_dendro(n = coucou,merge = Histo_final)
  
  
  
  
  # Sortie pour la fonction :
  return(list(tableau = A, merge = Histo_final, labels = labels, order = Depart2,height = sort(abs(hii),decreasing = FALSE )))
}

clust_khi_geo <- function(mots,nbgroupe,spdf_geo) {
  
  i<-which(colnames(DATA)== mots)
  spdf1<-lspdf[[i]]
  A<-inter_spdf(spdf1,2,spdf_geo,2)$A
  A <- t(A)
  dep <- A
  khi_b <- chisq.test(dep)$statistic
  # Renommage de la matrice A
  Nb_mots <- length(A[,1])
  coucou <- Nb_mots
  Nb_region <- length(A[1,])
  nom_col <- as.vector(seq(-1,-Nb_region,-1))
  nom_row <- as.vector(seq(-1,-Nb_mots,-1))
  colnames(A) <- nom_col
  rownames(A) <- nom_row
  
  # Nombres de tour
  combien <- Nb_mots - nbgroupe
  
  #  Variable utile pour la suite
  valeur_des_khi <- c()
  Matrice_de_cont <- c()
  nom_de_la_variable_cree <- 1
  nb_combi <- (Nb_mots*(Nb_mots+1)/2)-Nb_mots
  historique <- matrix(NA,nrow = nb_combi,ncol = 2)
  Histo_final <- matrix(NA,nrow = combien,ncol = 2)
  possition <- 1
  test <- 1
  labels <- rownames(dep)
  hii<-c()
  
  #dÃ©but la boucle
  
  #Boucle qui calcule toute les unions possible et par la suite calcule le nouveau khi-deux
  
  
  #Boucle pour savoir le nombre de regroupement que l'on veut faire
  for(nb in 1:combien) {
    
    historique <- matrix(NA,((Nb_mots*(Nb_mots+1)/2)-Nb_mots),2)
    #Savoir le nombre de mots et le mots et le nombre de rÃ©gions
    Nb_mots <- length(A[,1])
    
    #Boucle pour sÃ©lÃ¨ctionner l'ensemble des paires  calculer
    for (i in 1:Nb_mots) {
      for (p in 1:Nb_mots){
        if (i == p) {
          NULL
        }
        else if (p < i){
          NULL
        }
        else {
          
          # Historique de touts les paires de regroupement
          
          nom_des_row <- rownames(A)
          nom_des_col <- rownames(A)
          historique[test,1] <- nom_des_row[i]
          historique[test,2] <- nom_des_col[p]
          test <- test +1
          
          # Addition des deux Khi-deux
          cumule <- A[i,] + A[p,]
          
          #Suppresion des deux  lignes ajoutÃ©es dans l'Ã©tapes prÃ©cÃ©dante
          nom <- rownames(A)
          nom <- nom[c(-i,-p)]
          Abis <- (A[c(-i,-p),]) 
          
          
          
          #CrÃ©ation de la nouvelle matrice avec le cumule des deux lignes i et j
          Nouveau_matrice_de_cont <- (rbind(cumule,Abis))
          
          # Changement du nom de lignes
          rownames(Nouveau_matrice_de_cont)[rownames(Nouveau_matrice_de_cont)=="cumule"] <- as.character(possition)  
          
          #Cas de l'avant dernier  groupe 
          rownames(Nouveau_matrice_de_cont)[rownames(Nouveau_matrice_de_cont)=="Abis"] <- as.character(nom)
          
          rm(nom)
          #Calcule du Khi-Deux sur la matrice de la ligne d'avant
          
          Khi_deux_bis <- chisq.test(Nouveau_matrice_de_cont)$statistic
          valeur_des_khi <- rbind(valeur_des_khi,Khi_deux_bis)  
          
          #RÃ©cuperation des matrices de contingence de toutes les combinaison
          Matrice_de_cont <- rbind(Matrice_de_cont,Nouveau_matrice_de_cont)
        }
      }
    } 
    
    #Traitement des Khi-deux
    valeur_des_khi <- as.vector(valeur_des_khi)
    possition_du_max <- which.max(valeur_des_khi)
    hii <- c(hii,(khi_b-valeur_des_khi[possition_du_max]))
    ligne_de_debut <- (Nb_mots-1) * possition_du_max
    
    #RÃ©cupÃ©ration du nouveau tableau de contingence
    A <- Matrice_de_cont[c(((ligne_de_debut+1)- (length(A[,1])-1) ):(ligne_de_debut)),]
    
    #gestion des historiques
    valeurs <- historique[possition_du_max,]
    Histo_final[nb,] <- as.numeric(valeurs)
    possition <- possition+1
    
    #RÃ©nisialisation des variables
    test <- 1
    valeur_des_khi <- c()
    possition_du_max <- c()
    valeur_des_khi <- c()
    ligne_de_debut <- c()
    Matrice_de_cont <- c()
    nom_des_row <- c()
    nom_des_col <- c()
    historique <- c()
    
    #Fin de la boucle
  }
  
  #Fin
  
  if (class(A) == "numeric") {
    A <- matrix(A,nrow = 1)
    rownames(A) <- coucou
  }
  
  
  # Ordre du dendrogramme :
  Depart2 <- calcul_ordre_dendro(n = coucou,merge = Histo_final)
  
  
  
  
  # Sortie pour la fonction :
  return(list(tableau = A, merge = Histo_final, labels = labels, order = Depart2,height = sort(abs(hii),decreasing = FALSE ) ))
}

clust_khi_notion <- function(mots,nbgroupe,spdf_geo) {
  
  i<-which(colnames(DATA)== mots)
  spdf1<-lspdf[[i]]
  A<-inter_spdf(spdf1,2,spdf_geo,2)$A
  dep <- A
  khi_b <- chisq.test(dep)$statistic
  # Renommage de la matrice A
  Nb_mots <- length(A[,1])
  coucou <- Nb_mots
  Nb_region <- length(A[1,])
  nom_col <- as.vector(seq(-1,-Nb_region,-1))
  nom_row <- as.vector(seq(-1,-Nb_mots,-1))
  colnames(A) <- nom_col
  rownames(A) <- nom_row
  
  # Nombres de tour 
  combien <- Nb_mots - nbgroupe
  
  # Variable utile pour la suite 
  valeur_des_khi <- c()
  Matrice_de_cont <- c()
  nom_de_la_variable_cree <- 1
  nb_combi <- (Nb_mots*(Nb_mots+1)/2)-Nb_mots
  historique <- matrix(NA,nrow = nb_combi,ncol = 2)
  Histo_final <- matrix(NA,nrow = combien,ncol = 2)
  possition <- 1
  test <- 1
  labels <- rownames(dep)
  hii<-c()
  
  # debut la boucle
  
  #Boucle qui calcule toute les unions possible et par la suite calcule le nouveau khi-deux
  
  
  #Boucle pour savoir le nombre de regroupement que l'on veut faire
  for(nb in 1:combien) {
    
    historique <- matrix(NA,((Nb_mots*(Nb_mots+1)/2)-Nb_mots),2)
    #Savoir le nombre de mots et le mots et le nombre de regions
    Nb_mots <- length(A[,1])
    
    #Boucle pour selèctionner l'ensemble des paires  calculer
    for (i in 1:Nb_mots) {
      for (p in 1:Nb_mots){
        if (i == p) {
          NULL
        }
        else if (p < i){
          NULL
        }
        else {
          
          # Historique de touts les paires de regroupement
          
          nom_des_row <- rownames(A)
          nom_des_col <- rownames(A)
          historique[test,1] <- nom_des_row[i]
          historique[test,2] <- nom_des_col[p]
          test <- test +1
          
          # Addition des deux Khi-deux
          cumule <- A[i,] + A[p,]
          
          #Suppresion des deux  lignes ajoutees dans l'etapes precedante
          nom <- rownames(A)
          nom <- nom[c(-i,-p)]
          Abis <- (A[c(-i,-p),]) 
          
          
          
          #Creation de la nouvelle matrice avec le cumule des deux lignes i et j
          Nouveau_matrice_de_cont <- (rbind(cumule,Abis))
          
          # Changement du nom de lignes
          rownames(Nouveau_matrice_de_cont)[rownames(Nouveau_matrice_de_cont)=="cumule"] <- as.character(possition)  
          
          #Cas de l'avant dernier  groupe 
          rownames(Nouveau_matrice_de_cont)[rownames(Nouveau_matrice_de_cont)=="Abis"] <- as.character(nom)
          
          rm(nom)
          #Calcule du Khi-Deux sur la matrice de la ligne d'avant
          
          Khi_deux_bis <- chisq.test(Nouveau_matrice_de_cont)$statistic
          valeur_des_khi <- rbind(valeur_des_khi,Khi_deux_bis)  
          
          #Recuperation des matrices de contingence de toutes les combinaison
          Matrice_de_cont <- rbind(Matrice_de_cont,Nouveau_matrice_de_cont)
        }
      }
    } 
    
    #Traitement des Khi-deux
    valeur_des_khi <- as.vector(valeur_des_khi)
    possition_du_max <- which.max(valeur_des_khi)
    hii <- c(hii,(khi_b-valeur_des_khi[possition_du_max]))
    ligne_de_debut <- (Nb_mots-1) * possition_du_max
    
    #Recuperation du nouveau tableau de contingence
    A <- Matrice_de_cont[c(((ligne_de_debut+1)- (length(A[,1])-1) ):(ligne_de_debut)),]
    
    #gestion des historiques
    valeurs <- historique[possition_du_max,]
    Histo_final[nb,] <- as.numeric(valeurs)
    possition <- possition+1
    
    #Renisialisation des variables
    test <- 1
    valeur_des_khi <- c()
    possition_du_max <- c()
    valeur_des_khi <- c()
    ligne_de_debut <- c()
    Matrice_de_cont <- c()
    nom_des_row <- c()
    nom_des_col <- c()
    historique <- c()
    
    #Fin de la boucle
  }
  
  # Fin
  
  if (class(A) == "numeric") {
    A <- matrix(A,nrow = 1)
    rownames(A) <- coucou
  }
  
  
  # Ordre du dendrogramme :
  Depart2 <- calcul_ordre_dendro(n = coucou,merge = Histo_final)
  
  
  
  
  # Sortie pour la fonction :
  return(list(tableau = A, merge = Histo_final, labels = labels, order = Depart2,height = sort(abs(hii),decreasing = FALSE)))
}

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

Entropie <- function(A) {
  #A est le tableau de contingence des aires 
  somme <- sum(A)
  proba_de_Mair <- A/somme
  
  vecteur_de_proba <- as.vector(proba_de_Mair)
  prob <- vecteur_de_proba[vecteur_de_proba!=0]
  entropiex <- sum(-prob*log(prob))
  return(entropiex)
}

inter_spdf<-function(spdf1, idx1, spdf2, idx2){
  
  p<-length(spdf1)
  q<-length(spdf2)
  A<-matrix(0,p,q)
  B<-matrix(0,p,q) #nombre de morceaux dans l'intersection
  
  for(i in 1:p)
    for(j in 1:q)
    {
      scinter<-gIntersection(spdf1[i,],spdf2[j,], byid=TRUE)
      if(!is.null(scinter))
      {
        if(class(scinter)=='SpatialCollections')
          if(!is.null(scinter@polyobj))
          {
            spinter<-scinter@polyobj  
            A[i,j]<-sum(area(spinter))/1e6
            B[i,j]<-length(spinter)
          }
        if(class(scinter)=='SpatialPolygons')
        {
          spinter<-scinter
          A[i,j]<-sum(area(spinter))/1e6
          B[i,j]<-length(spinter)
        }
      }
    }
  
  #NOMMAGE des lignes et colonnes de A et B
  
  rownames(A)<-spdf1@data[[idx1]]
  colnames(A)<-spdf2@data[[idx2]]
  rownames(B)<-spdf1@data[[idx1]]
  colnames(B)<-spdf2@data[[idx2]]
  
  output<-list(A=A, B=B)
  
  return(output)
  
}

map_clust <- function(cut, spdf1){
  
  pol_base <- list()
  test <- cut
  for (i in (1:length(spdf1@plotOrder))){
    pol_base[[i]] <- spdf1@polygons[[i]]@Polygons
    spdf1@polygons[[i]]@Polygons <- list()
  }
  
  for (j in (1:max(test))){
    for (k in (1:length(test))){
      
      if (test[k] == j) {
        spdf1@polygons[[j]]@Polygons <- append(spdf1@polygons[[j]]@Polygons,pol_base[[k]])
      }
    }
  }
  spdf1@plotOrder <- sort(spdf1@plotOrder)
  palette<-rainbow(nrow(spdf1@data))
  plot(spdf1,col = rainbow(max(test)))
  plot(spdf1,lwd=2,add=TRUE)
  legend("topright",legend=spdf1$lemme,fill=rainbow(max(test)),ncol=3)
}

