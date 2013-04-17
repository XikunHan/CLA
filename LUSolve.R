LUSolve = function(A, b, tol=10^-8){
  n = nrow(A)
  
  LU = LU(A, tol) #Get UL Factorization of A
  L = LU$L
  U = LU$U
  
  #Step 1: Lc = b
  c = rep(0,n)
  c[1] = b[1]/L[1,1]  # Fill in the 1st value of c
  for (j in 2:n ) {
    back = 0                  #  compute x[j] by back substitution  å
    #This is the vectorized version.
    LT = L[j,1:(j-1)]
    cT = c[1:(j-1)]
    back = LT %*% cT
    c[j] = (b[j] - back)/L[j,j]
  }
  
  print("Step 1 Finshied c = ")
  print(c)
  
  #Step 2: Ux = c
  x = rep(0,n)        # pre-allocate a vector x with 0s in it.
  x[n] = c[n]/U[n,n]  # Fill in the nth value of x
  for (j in (n-1):1 ) {
    back = 0                  #  compute x[j] by back substitution  
    #This is the vectorized version.
    UT = U[j,(j+1):n]
    xT = x[(j+1):n]
    back = UT %*% xT
    x[j] = (c[j] - back)/U[j,j]
  }
  
  print("Step 2 Finished x = ")
  
  return(x)
}