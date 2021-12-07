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