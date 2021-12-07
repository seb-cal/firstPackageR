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