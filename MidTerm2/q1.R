## This R file contains solution for Exam 2 question 1

###################### Setting up ####################
f = function(x){exp(-3*x)*sin(2*pi*x)} #define the function

##################### (a) interploating polynomial ######################
xx = seq(0,1,length=1000)
plot(xx,f(xx),type='l')

x = seq(0,1,length=6) #sampling 6 points for 5 degree polynomial
y = f(x)
points(x,y,pch=19,col='red')
grid()

print("(a) Interploating Polynomial")
print("x=")
print(x)
print("y=")
print(y)

c = NewtonDD(x,y) 
p = function(z) { 
  Horner(c,z,x) 
}
print("Coefficients are")
print(c)

lines(xx,p(xx),col='blue')

# Checking Error
error = vnorm(f(xx)-p(xx))
print("Error for Interploating Polynomial")
print(error)

##################### (b) least square fitting ##########################
# This fits a linear model using least square
# k is the number of degree
# x is the x coordinate of the points
# y is the y coordinate of the points
k = 5
x = seq(0,1,length=20)
y = f(x)

print("(b) Least Square Fitting")
print("x=")
print(x)
print("y=")
print(y)

xx = seq(0,1,length=1000)
plot(xx,f(xx),type='l')

points(x,y,pch=19,col='red')
grid()

A = matrix(nrow=length(x),ncol=(k+1)) #be aware of the redundancy
for (i in 0:k)  A[,i+1] = x^(i) # set up A
print("Condition Number for t(A) %*% A")
print(Cond(t(A) %*% A))

coeffs = solve(t(A)%*%A, t(A)%*%y) # this x bar, normal equation
print("polynomial coefficients:")
print(t(coeffs))
q = function(z) Horner(coeffs,z) # this is used for ploting 
lines(xx,q(xx),col='blue')

bhat = A %*% coeffs  # the predicted values 
r = y - bhat # r: the residuals 

# Squared Error 
print("squared error of the model, SE:")
SE = t(r) %*% r
print(c(SE,sqrt(SE)))

# Root mean Squared Error, RMSE 
print("RMSE:")
RMSE = sqrt(SE/length(r))
print(c(RMSE))

# 2-norm error
# Checking Error
error = vnorm(f(xx)-q(xx))
print("2-norm error for Least Square")
print(error)

####################### (c) Cubic spline ###########################
x = seq(0,1,length=6) #sampling 6 points for 5 degree cubic spline
y = f(x)

xx = seq(0,1,length=1000)
plot(xx,f(xx),type='l')
points(x,y,pch=19,col='red')
grid()

print("(c) Cubic spline Fitting")
print("x=")
print(x)
print("y=")
print(y)

result = spline(x,y,n=1000,method="fmm")
lines(result,col='blue')

# Checking Error
error = vnorm(f(xx)-result$y)
print("Error for Cubic Spline")
print(error)
