fourier = function(n,inv=FALSE) {
  if (inv) 
    w = complex(real = cos(2*pi/n), imaginary =  sin(2*pi/n))
  else 
    w = complex(real = cos(2*pi/n), imaginary =  -sin(2*pi/n))
  ww = w^(0:(n-1))
  FF = matrix(nrow=n,ncol=n)
  for (i in 1:n) {
    FF[,i] = ww^(i-1)
  }
  return(1/sqrt(n)*FF) # We are rescaling here
}

DFT = function(x,inv=FALSE) {
  n = length(x)
  FF = fourier(n,inv)  
  return(FF %*% x)
}