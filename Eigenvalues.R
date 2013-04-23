library("Matrix", lib.loc="/Library/Frameworks/R.framework/Versions/2.15/Resources/library")
A = cbind(c(-17,-30,-3,-6), c(14.5,22.5,-2.0,0.5),c(-16,-26,0,-2),c(3.5,8.5,5,6.5))
print(A)

#  Power Iteration ------------------------------
# k is the max number of iteration
PI = function(A,x = seq(1,nrow(A)),k=100,tol=.000005) {
  for (j in 1:k) {
    u = x/vnorm(x)
    x = A %*% u
    new = x/vnorm(x)
    if (vnorm(new-u,'I')<tol) break
  }
  lambda = (t(u) %*% x)[1]
  u = x/vnorm(x)
  return(list(val=lambda,vec=u,steps = j))
}

#  Inverse Power Iteration, with shifting-------------
IPI = function(A,x = seq(1,nrow(A)),s=0,k=200,tol=.000005) {
  n = nrow(A)
  As = A - s*diag(n)
  for (j in 1:k) {
    u = x/vnorm(x)
    x = solve(As,u)
    lambda = (t(u) %*% x)[1]
    new = x/vnorm(x)
    if (vnorm(new-u,'I')<tol || vnorm(new+u,'I')<tol) break
  }
  u = x/vnorm(x)
  lambda = 1/lambda + s
  
  return(list(val=lambda,vec=u,steps = j))
}


#  Rayleigh Quotient Iteration -------------

RQI = function(A,x = seq(1,nrow(A)),k=10,tol=.5) {
  n = nrow(A)
  for (j in 1:k) {
    u = x/vnorm(x)
    lambda = (t(u) %*% A %*% u)[1]
    As = A - lambda*diag(n)
    if (rankMatrix(As)<n) break
    x = solve(As,u)
  }
  u = x/vnorm(x)
  lambda = t(u) %*% A %*% u
  return(list(val=lambda,vec=u,steps = j))
}