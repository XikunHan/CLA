elim = function (A,tol=10^-8){
  n=nrow(A)
  for (j in (1:(n-1))){
    pivot = A[j,j]
    if (abs(pivot)<tol) stop('zero pivot encountered')
    for (i in (j+1):(n)) {
      A[i,]=A[i,] - (A[i,j]/pivot)*A[j,]
    }
  }
  return(A)
}