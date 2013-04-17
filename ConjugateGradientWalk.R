A = cbind(c(3,2),c(2,6))
b = c(2,-1)
guess = c(-5,1)
steps = 5

ContourGrad = function(x1,x2) { 
  x = matrix(c(x1,x2),nrow=2)
  y =  .5* t(x) %*% A %*% x - t(b) %*% x 
}

xx = seq(  -6, 6, by = .1)
yy = seq(-6, 6, by = .1)
zz = matrix(NA,nrow=length(xx),ncol=length(yy))
for (i in (1:length(xx))) {
  for (j in (1:length(yy)))
    zz[i,j] = ContourGrad(xx[i],yy[j])
}
ResidualWalk = function(A,b,x = rep(0,length(b)),step=.01,n=20) {
  # Walk in the direction of the residual
  # Inputs: symm pos def matrix A, rhs b, number of steps n
  # Output: solution x to Ax=b
  x = cbind(x)
  r = b - A %*% x
  hist = matrix(NA,nrow=2,ncol=n+1)
  hist[,1] = x
  for (i in 1:n ) {
    if (max(abs(r)) < 2*10^-16) break
    x = x + step/(sqrt(t(r) %*% r)[1,1]) * r               # take step
    r = b - A %*% x
    hist[,i+1] = x
  }
  return(list(x=x,history=hist))
}


contour(xx,yy,zz)
xstar = solve(A,b)
points(xstar[1],xstar[2],col='purple',pch="*",cex=2)

hh = ResidualWalk(A,b,x=guess,step=.25,n=steps)$history
xxx = hh[1,]
yyy = hh[2,]
lines(xxx,yyy,col='blue')
points(xxx,yyy,col='red',pch=20)

hh = ConjGrad(A,b,x=guess,history=TRUE)$history
xxx = hh[1,]
yyy = hh[2,]
lines(xxx,yyy,col='blue')
points(xxx,yyy,col='red',pch=20)