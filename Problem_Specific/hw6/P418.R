# HW6. Problem 4. Modeled after Problem 4.1.8(a) 9(a) and 10(a) in the book

###################### Setting up Points #####################
x = c(0,1,2,5)
y = c(0,3,3,6)

##################### Fitting Function ######################
# This function fits a linear model using least square
# k is the number of degree
# x is the x coordinate of the points
# y is the y coordinate of the points
fitting = function(k=1,x,y){
  plot(x,y,pch=19,col='red')
  grid()
  
  A = matrix(nrow=length(x),ncol=(k+1)) #be aware of the redundancy
  for (i in 0:k)  A[,i+1] = x^(i) # set up A
  print("Condition Number for t(A) %*% A")
  print(Cond(t(A) %*% A))
  
  coeffs = solve(t(A)%*%A, t(A)%*%y) # this x bar, normal equation
  print("polynomial coefficients:")
  print(t(coeffs))
  q = function(z) Horner(coeffs,z) # this is used for ploting 
  xx = seq(x[1]-1,x[length(x)]+1,len=1000)
  lines(xx,q(xx),col='blue')
  
  bhat = A %*% coeffs  # the predicted values 
  r = y - bhat # r: the residuals 
  
  # Squared Error ------------------------------------------------
  print("squared error of the model, SE:")
  SE = t(r) %*% r
  print(c(SE,sqrt(SE)))
  
  # Root mean Squared Error, RMSE --------------------------------
  print("RMSE:")
  RMSE = sqrt(SE/length(r))
  print(c(RMSE))
  
  return(list(coeffs=coeffs, A=A))
}

##################### Calling Fitting Function ##################
fitting(k=1,x=x,y=y) #8(a) line
fitting(k=2,x=x,y=y) #9(a) parabola
fitting(k=3,x=x,y=y) #10(a) degree 3 polynomial

xx = seq(x[1]-1,x[length(x)]+1,len=1000)
yy = Interpolator(xx=x,yy=y,x=xx)
lines(xx,yy,col='green') #10(a) degree 3 interploation

