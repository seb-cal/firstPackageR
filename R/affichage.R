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