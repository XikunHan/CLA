############################### SVDApprox(A,k) #######################################
# return sum of 1st k rank-1 matrices in spectral decomp
# kth spectral approx of A
# good default for k:
#   if k is tall or wide, we can't just do nrow(A)/2
SVDApprox = function(A, k = floor(min(nrow(A),ncol(A))/2)){
  foo = svd(A)
  sings = foo$d
  U = foo$u
  V = foo$v
  M = matrix(0,nrow=nrow(A),ncol=ncol(A)) # same size as A
  
  for(i in (1:k)){
    M = M+sings[i] * U[,i] %*% t(V[,i]) 
  }
    
  return(M)
}

############################### Testing ##########################################
f = function(t) {3*cos(2*pi*t/10) + 2*sin(7*2*pi*t/10) + .5*cos(4*2*pi*t/10)}
t = seq(0,10,length=2^8)
x = f(t)
plot(t,x,type='l')
grid()

# part 6
y = DFT(x)
plot(abs(y))
# part 7
z = DFT(y,inv=TRUE) 
plot(abs(z))