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
