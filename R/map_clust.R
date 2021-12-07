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