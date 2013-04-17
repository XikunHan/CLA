UnitCircleMap = function(A) {
  t = seq(0,2*pi,len=1000)
  x = cos(t)
  y = sin(t)
  pts = A %*% t(cbind(x,y))
  
  newx = pts[1,]
  newy = pts[2,]
  
  M = max(c(newx,newy,1.5))
  m = min(c(newx,newy,-1.5))
  
  plot(x,y,type='l',col='black',xlim=c(m,M),ylim=c(m,M))
  lines(newx,newy,col='red')
  
  #text(0,.8*M,kappa(A))
  foo = svd(A)
  nn=round(100*max(foo$d))/100
  cc=round(100*max(foo$d)/min(foo$d))/100
  #text(-.5*M,.9*M,"norm_2(A)="); text(0,.9*M,nn)
  text(-.5*M,.9*M,"Cond_2(A)="); text(0,.9*M,cc)
  
}

#  Note the maximum expansion of the unit circle in the 2-norm
A21 = cbind(c(3,4),c(1,1))
A22 = cbind(c(.3,-1),c(1.2,.3))
A23 = cbind(c(2.1,-1),c(-1.2,.5))
A24 = cbind(c(21,-10),c(-12,15))