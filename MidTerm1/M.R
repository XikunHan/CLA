M = function(tau = 0.5,n = 500) {
  A = diag(n) #assign a square identity matrix of size n*n to A
  for (i in 1:(n-1)) {
    for (j in (i+1):n) { #loop through all the non-diagonal entry
      r = runif(1,min=-1,max=1) #choose a random number r
      if (abs(r)< tau) { #if |r| < tau
        A[i,j] = r #setting entry to r
        A[j,i] = r #setting the symmetric entry to r
      } #other entries are 0 by default
    }
  }
  return(A)
}