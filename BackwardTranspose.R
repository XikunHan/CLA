#This function calcualtes the transposes of a matrix over the backward diagonal
backT = function(A){
  nr = nrow(A)
  nc = ncol(A)
  B = matrix(0,nr,nc)
  for (i in (1:nr)){
    for (j in (1:nc)){
      B[i,j] = A[nr-j+1, nc-i+1]
    }
  }
  return(B)
}