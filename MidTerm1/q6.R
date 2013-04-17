# This function accepts n as an input and 
# returns the value x for which the max eigenvalue of
# Tn(x) is 2013
g = function(n){
  f = function(x){
    max(eigen(T(x,n))$value) - 2013 
  }
  #Because x > 0, its safe to start with x=1
  #using Newton's method
  result = Newton(f,maxiters=1000,x=1,tol=10^-5)
  return(result)
}

#We need to product xseq and yseq for R's plot function
xseq = seq(2,20)
yseq = rep(0,20-2+1)
for (i in (2:20)){
  yseq[i-1] = g(i)$root
}
plot(xseq,yseq,xlab='n',ylab='x')

print("Solution for integer n=2 to 20:")
print(yseq)

