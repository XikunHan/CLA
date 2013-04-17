#################Chebychev Function#####################
Chebychev = function(a,b,n){
  result = (a+b)/2 + ((b-a)/2)*cos((2*(1:n)-1)*pi/(2*n))
  return (result)
}

################Interpolation#########################
#f = function(x) {sin(x)}
f = function(x) {1/(1+12*x^2)}

a = -2
b = 2

#we can zoom out here
aa = -5
bb = 5


#xx = seq(a,b,length=1000) 
xx = seq(aa,bb,length=1000) #change this to zoom out
plot(xx,f(xx),type='l')

n = 50 # number of points
#x = seq(a,b,length=n) #pick n evenly spaced points, uncommon to check error
x = Chebychev(a,b,n)
y = f(x)
points(x,y,pch=20,col='tomato')

c = NewtonDD(x,y) # coefficients we need to feed into Horner's method
p = function(z) { # this is the new function we have
  Horner(c,z,x) # we need to feed x to Horner's if we want to use Newton
}

#This polynormial should make senses for other points
lines(xx,p(xx),col='magenta') #noted, Horner's method is vectorized

################Interpolation Error###############
maxerr = 1/2^(n-1)*((b-a)/2)^n/factorial(n)
print(maxerr)

#It is wrong to check the following
#Because we are trying to check the points we just fitted
#max(abs(f(x)-p(x)))

#Make a new sequence!
xxx = seq(-2,2,length = 1000) #remember we cannot check everything
error = max(abs(f(xxx)-p(xxx)))
print(error) #noted it does not have to be the same as maxerr



