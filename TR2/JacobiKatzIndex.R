# Jacobi Method for solving a system of equations Bx=d for Katz Index
jacobi = function(A,b,x = rep(0,length(b)),m=1000,tol=10^-4, verbose=FALSE) { # x is a vector of initial guess
  n = nrow(A)
  d = diag(A)
  R = A
  R[cbind((1:n),(1:n))] = 0  # allows for r to be sparse if A is sparse
  
  p = 2 #vnorm() uses 2 norm by default, but we can change this
  count = 0
  while(count<m && vnorm((b-A%*%x),p) > tol*vnorm(b,p)){ 
    #two stopping conditions: 
    #maximum iter reach
    #and relative backward error is small enough for tolerance
    count=count+1
    x = (b - R%*%x)/d
    if (verbose) {
      print("Current Step: ")
      print(count)
      print("The solution for x")
      print(x)
    }
  }
  
  print("Number of Steps: ")
  print(count)
  return(x)
}
