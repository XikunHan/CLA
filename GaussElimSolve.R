MySolve = function(A,b,tol=10^-8) {
  n = nrow(A)
  A = cbind(A,b)      # append b to the right of A
  A = elim(A, tol)    # Gaussian elimination below the main diagonal
  b = A[,n+1]         # Get the row reduced version of b back
  x = rep(0,n)        # pre-allocate a vector x with 0s in it.
  x[n] = b[n]/A[n,n]  # Fill in the nth value of x
  for (j in (n-1):1 ) {
    back = 0                  #  compute x[j] by back substitution  
    for ( i in n:(j+1) ) {           #  with an inner for loop
      back = back + A[j,i]*x[i] 
    }
    x[j] = (b[j] - back)/A[j,j]
  }
  return(x)
}