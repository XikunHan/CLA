Vandermonde = function(x){
  n = length(x)
  V = matrix(0, nrow=n, ncol=n)
  
# double for loop solution
#   for(i in 1:n)
#     for(j in 1:n)
#       V[i,j] = x[i]^(j-1)

# vectorized solution
   for(i in 1:n)
     V[i,] = x[i]^seq(0,n-1)
  
# another vectorized solution
#  for (j in 1:n)  
#    V[,j] = x^(j-1)
  return(V)
}

VandermondeInterpolator = function(x,y){
  n = length(x)
  V = Vandermonde(x)
  c = solve(V,y) # c contains all the coefficients
  
  p = function(z) { 
    c %*% z^(0:(length(c)-1))
  }
  
  #plot(x,y,pch=20, col='red') we plot the points later
  xx = seq(min(x)-1,max(x)+1,length=1000)
  #lines(xx, p(xx)) you can't do this because p(xx) is not vectorized
  yy = rep(0, length(xx))
  
  for(i in 1:length(xx))
    yy[i] = p(xx[i])
  
  plot(xx,yy,type='l',col='blue')
  
  points(x,y,pch=20, col='red') 
  
  return(c)
}

x = c(-8,1,2,3,4)
y = c(0,10,6,4,10)
VandermondeInterpolator(x,y)
  
