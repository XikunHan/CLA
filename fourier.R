fourier = function(n, inv=FALSE){
  w = complex(real=cos(-2*pi/n),imaginary=sin(-2*pi/n))
  V = matrix(0, nrow=n, ncol=n) 
  ww = w^seq(0,n-1)
  
  # vectorized solution
  for (j in 1:n){ 
    if(inv)
      V[,j] = ww^(1-j)
    else
      V[,j] = ww^(j-1)
  }
  
  return((1/sqrt(n))*V)  
}



