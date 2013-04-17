A = diag(c(.84,.94))
B = cbind(c(1,1),c(-1,2))
A2 = B %*% A %*% solve(B)

A1 = rbind(c(1.0633,.0367),c(.0733,1.0267))
A2 = rbind(c(0.8733, -0.0333),c(-0.0666, 0.906))
A3 = rbind(c(0.809, 0.587),c(-0.587, 0.809))


myplot = function(T, ...) {  
  plot(T[1,],T[2,],pch='.',xlab='x',ylab='y',...)
  grid()
}

T= rbind(runif(1000,min=-1,max=1),runif(1000,min=-1,max=1))


T = A1 %*% T; myplot(T,xlim=c(-10,10),ylim=c(-10,10))

# T = A2 %*% T; myplot(T,xlim=c(-1,1),ylim=c(-1,1))
# T = A3 %*% T; myplot(T,xlim=c(-1.8,1.8),ylim=c(-1.8,1.8))
# 
# 