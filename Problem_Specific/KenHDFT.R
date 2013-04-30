############################# Fourier Function ############################
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

############################# DFT Function ############################
DFT = function(x,inv=FALSE){
  return(fourier(length(x),inv) %*% x)
}

############################## Testing #################################

## part 5
f = function(t) {3*cos(2*pi*t/10) + 2*sin(7*2*pi*t/10) + .5*cos(4*2*pi*t/10)}
t = seq(0,10,length=2^8)
x = f(t)
plot(t,x,type='l')
grid()

## part 6
y = DFT(x)
plot(abs(y))

## part 7 got back to where you started
z = DFT(y,inv=TRUE)
plot(abs(z))
