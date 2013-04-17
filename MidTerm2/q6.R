##This file contain solution for Exam 2 Question 6

####################### My2DSpline function ##################
My2DSpline = function(x,y,z,m){
  xn = length(x)
  yn = length(y)
  xx = seq(x[1],x[xn],length=m)
  yy = seq(y[1],y[yn],length=m)
  
  M = matrix(nrow=length(x),ncol=m)
  for (i in 1:nrow(z)) {
    M[i,] = spline(x,z[i,],xout=xx)$y
  }
  
  MM = matrix(nrow=m,ncol=m)
  for (j in 1:ncol(M)) {
    MM[,j] = spline(y,M[,j],xout=yy)$y
  }
  
  return(list(x=xx,y=yy,z=MM))
}

#################### Matrix Norm Function #################
mnorm = function(A,p='I'){
  if(p=='I'){ #maximum absolute row sum
    max = 0;
    for(i in 1:nrow(A)){
      sum = sum(abs(A[i,]))
      if(sum>max){
        max = sum
      }
    }
    return(max)
  } else if(p==1){ #maximum absolute column sum
    max = 0;
    for(i in 1:ncol(A)){
      sum = sum(abs(A[,i]))
      if(sum>max){
        max = sum
      }
    }
    return(max)
  }
  return(-1)  
}

######################### Question (a) #####################
x = seq(0,3,length=7)
y = seq(0,3,length=7)
A = rbind(
  c(0,0,0,1,0,0,0),
  c(0,0,0,2,0,0,0),
  c(0,0,2,4,2,0,0),
  c(1,2,4,8,4,2,2),
  c(0,0,2,4,2,0,0),
  c(0,0,0,2,0,0,0),
  c(0,0,0,1,0,0,0))

persp(x,y,A)
image(x,y,A)

result = My2DSpline(x,y,A,60)
persp(result)
image(result)

######################### Question (b) ######################
f = function(x,y){exp(-(x^2+y^2))}
x = seq(-1,1,length=10)
y = seq(-1,1,length=10)
z = matrix(0,nrow=10, ncol=10)
for(i in 1:10){
  for(j in 1:10){
    z[i,j] = f(x[i],y[j])
  }
}

result = My2DSpline(x=x,y=y,z=z,m=100)
persp(result)
image(result,xlab='Cubic Spline Fit')
# Plotting all the sampling points
for(i in 1:10){
  for(j in 1:10){
    points(x[i],y[j],pch=16)
  }
}

# Comparing actual value to our cubic spline fit
zz = matrix(0,nrow=100,ncol=100)
xx = result$x #use the same sampling points
yy = result$y
for(i in 1:100){
  for(j in 1:100){
    zz[i,j] = f(xx[i],yy[j])
  }
}

# find max norm of the matrx diff
diff = zz-result$z #find difference matrix
error = max(abs(diff))
print("The max norm of the matrix diff is ")
print(error)
print("The infinit matrix norms")
print(mnorm(A=diff))

# contour plot of original function
image(xx,yy,zz, xlab='Original Function')
# Plotting all the sampling points
for(i in 1:10){
  for(j in 1:10){
    points(x[i],y[j],pch=16)
  }
}


