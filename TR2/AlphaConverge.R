#### The same A we used for network influence example
ii = c(1,2,2,3,4,4,4,5,5,5,6,6,6,7,7,7,8,9,10,10,10,11,12,13,14,14,14)
jj = c(2,1,3,1,1,3,5,1,3,4,1,2,7,1,2,6,4,4,4,8,9,5,5,5,2,6,7)
n = 14
xx = rep(1,length(jj)) #all nonzero vectors are 1
A = spMatrix(nrow=n,ncol=n,i=ii,j=jj,x=xx)

# This function finds the alpha value within the tolerance
# when Katz Status index for matrix M converges
# a, b are the left and right bound for alpha
convergeAlpha = function(a,b,M=A,tol=10^-3){
  #tolerance for when to stop
  while (abs(a-b) > tol){
    mid = (a+b)/2
    if(doesConverge(mid, M)){
      a = mid
    } else {
      b = mid
    }
    print("Seaching")
    print(c(a,b))
  }
  return ((a+b)/2)
}

# This function checks whether the eigenvalues of a*M
# is less than 1
# Noted that the complex eigenvalues cannot be checked directly
doesConverge = function(a,M){
  eigens = eigen(a*M)$values
  for (i in (1:length(eigens))){
    r = 0
    if(!is.complex(eigens[i])) # complex value does not work for comparsion
      r = abs(eigens[i])
    else
      r = Re(eigens[i]) #real part of the complex number
    if (r >= 1)
      return(FALSE)
  }
  return(TRUE)
}

print(convergeAlpha(0.7,0.8))