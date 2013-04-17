# Customized Jacobi Method for solving Computer Problem 2.5:1 and 2
# ox is the original solution
# m is the maximum iteration
# tol is the tolerance
jacobiCP_2_5_2 = function(A,b,ox=cbind(rep(1,length(b))),m=25,tol=10^-6) {
  n = nrow(A)
  x = rep(0,length(b))  # initial guess is the all 0's vector
  d = diag(A)
  R = A
  R[cbind((1:n),(1:n))] = 0  # allows for r to be sparse if A is sparse
  
  p = 'I' #vnorm() uses infinit norm by default, but we can change this
  count = 0
  
  #Noted that we keep forward error within the tolerance
  while(count<m && vnorm((ox-x),p) > vnorm(ox,p)*tol){ 
    count=count+1
    x = (b - R %*% x)/d
  }
  print("Number of Steps: ")
  print(count)
  print("Backward Error")
  be = vnorm((b-A %*% x),p)/vnorm(b,p)
  return(be)

  #print("Solution: ")
  #return(x) #we don't print the solution to save the space
}
