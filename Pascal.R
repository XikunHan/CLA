Pascal = function(n){
  P = matrix(0,nrow=n,ncol=n) 
  for(i in 1:n){
    for(j in 1:n){
      P[i,j] = C(i+j-2, i-1)
    }
  }
  return (P)
}