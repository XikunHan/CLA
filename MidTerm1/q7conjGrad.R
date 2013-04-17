ConjGradQ7 = function(A,b,x = rep(0,length(b)),tol=2*10^-16,m = length(b),history=FALSE) {
  # Modified Conjugate Gradient Method for Exam 1 Problem 7(b)
  # Inputs: symm pos def matrix A, rhs b, number of steps m, 
  # x is the initial guess, 
  # tol is a stopping condition on the size of the residual
  # history gives a full history
  # Output: solution x to Ax=b
  # 
  n = length(b)
  r = b - A %*% x
  d = r
  
  #this records the residuls for each step
  #noted that this method would give the answer before the m+1 step
  resids = rep(NA, m) 
  
  if (history) {
    hist = matrix(NA,nrow=n,ncol=m+1)
    hist[,1] = x
  }
  steps = 0
  for (i in 1:m ) {
    steps = i
    if (max(abs(r)) < tol) break
    alpha = (t(r) %*% r)/(t(d) %*% A %*% d)  # step length
    x = x + alpha[1,1] * d                  # take step 
    if (history) {hist[,i+1] = x[,1]}
    rold = r
    resids[steps] = vnorm(rold, 2)
    r = rold - alpha[1,1] * A %*% d         # new residual
    beta = (t(r) %*% r)/(t(rold) %*% rold)   # improvement this step
    d = r + beta[1,1]*d
  }
  
  resids[m] = vnorm(r, 2) #get the last residual
  
  print("Number of Steps:")
  print(steps)
  if (history) {return(list(x=x,history=hist))}
  else {return(list(x=x,steps=steps,resids=resids[!is.na(resids)]))}
}