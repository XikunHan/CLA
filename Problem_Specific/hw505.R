#################Chebychev Function#####################
Chebychev = function(a,b,n){
  result = (a+b)/2 + ((b-a)/2)*cos((2*(1:n)-1)*pi/(2*n))
  return (result)
}
 
################Defining function and bounds################
f = function(x) { cos(exp(x-3)/2) }
a = 2
b = 5.5

#we can zoom out here
aa = 1
bb = 8
 
xx = seq(aa,bb,length=1000) 
plot(xx,f(xx),type='l',xlab="x",ylab="y",xlim=c(a-.5,b+.5),ylim=c(-2,2),col='black',lwd=2)

###############Chebychev Interpolation###################
n = 25 # degree for Chebychev
x = Chebychev(a,b,n) #find Chevychev points
y = f(x)
points(x,y,pch=20,col='tomato')
# cc = NewtonDD(x,y) # coefficients for Chebychev
# pc = function(z) { # this is the interpolation function we have
#   Horner(cc,z,x) # we need to feed x to Horner's if we want to use Newton
# }

yp = Interpolator(x,y,x=xx)

lines(xx,yp,col='blue')

#############Evenly Spaced Interpolation###############
#redraw the original function
plot(xx,f(xx),type='l',xlab="x",ylab="y",xlim=c(a-.5,b+.5),ylim=c(-2,2),col='black',lwd=2)

#drawing interpolation
x = seq(a,b,length=n) #pick n evenly spaced points
y = f(x)
points(x,y,pch=20,col='tomato')

# ce = NewtonDD(x,y) # coefficients for evenly spaced
# pe = function(z) { # this is the interpolation function we have
#   Horner(ce,z,x) # we need to feed x to Horner's if we want to use Newton
# }

yp = Interpolator(x,y,x=xx)

lines(xx,yp,col='blue')

############Finding Error#########################
#noted: to find the error, we need to use 
#a and b instead of aa and bb as range
xx = seq(a,b,length=1000) 
yf = f(xx)

#Finding Error for evenly spaced
x = seq(a,b,length=n) #pick n evenly spaced points
y = f(x)
yp = Interpolator(x,y,x=xx)

err = yf-yp
print("The infinity norm of err for evenly space points is ")
print(vnorm(err, p='I'))

#Finding Error for Chebychev
x = Chebychev(a,b,n) #find Chevychev points
y = f(x)

yp = Interpolator(x,y,x=xx)

err = yf-yp
print("The infinity norm of err for Chebychev is ")
print(vnorm(err, p='I'))








