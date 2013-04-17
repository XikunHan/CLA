# Customized Jacobi Method for solving 2.5: 1
jacobi2_5_1 = function(A,b,m=5,tol=10^-5) {
  n = nrow(A)
  x = rep(0,length(b))  # initial guess is the all 0's vector
  d = diag(A)
  R = A
  R[cbind((1:n),(1:n))] = 0  # allows for r to be sparse if A is sparse
  
  p = 2 #vnorm() uses 2 norm by default, but we can change this
  count = 0
  while(count<m && vnorm((b-A %*% x),p) > tol*vnorm(b,p)){ 
    count=count+1
    x = (b - R %*% x)/d
    print(x) #print out vector x for each iteration
  }
  return(x)
}
