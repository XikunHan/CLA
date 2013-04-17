hilb = function(n){
  H = matrix(NA,nrow=n,ncol=n)
  for (i in 1:n) {
    for (j in 1:n) {
      H[i,j]= 1/(i+j-1)    
    }
  }
  return(H)
}