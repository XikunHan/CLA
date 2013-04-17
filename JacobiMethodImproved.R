# Improved Jacobi Method for solving a system of equations Ax=b
jacobi = function(A,b,m=25,tol=10^-4) {
  n = nrow(A)
  x = rep(0,length(b))  # initial guess is the all 0's vector
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
  }
  
  print("Number of Steps: ")
  print(count)
  return(x)
}
