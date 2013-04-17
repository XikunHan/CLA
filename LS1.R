# (1) The data -----------------------------------------------------
x = c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
y = c(3,2,2,1,1,1,0,0,0,0,2)
plot(x,y,col='red',pch=20,cex=2,xlim=c(-6,6),ylim=c(-2,4))
abline(0,0)
abline(v=0)
grid()

# # # (2) Interpolating polynomial: degree 10 -------------------------
# # coeffs = NewtonDD(x,y)
# # p = function(z) Horner(coeffs,z,x)
# # rxx = seq(-6,6,len=1000)
# # lines(xx,p(xx),col='black')
# 
# # # (3) Least Squares Fit: degree k  -------------------------------
k = 2
A = matrix(nrow=length(x),ncol=(k+1))
for (i in 0:k)  A[,i+1] = x^(i) # set up A

coeffs = solve(t(A)%*%A, t(A)%*%y) # this x bar
print("polynomial coefficients:")
print(t(coeffs))
q = function(z) Horner(coeffs,z) # this is used for ploting 
xx = seq(-6,6,len=1000)
lines(xx,q(xx),col='blue')

# # (3) plot bhat: the predicted values ------------------------------
bhat = A %*% coeffs
points(x,bhat,col='black',pch=19,cex=.5)

# # (4) plot r: the residuals ----------------------------------------
r = y - bhat
#this draw vertical lines connecting actual value and predicted value
for (i in 1:length(x)) lines(c(x[i],x[i]),c(y[i],bhat[i]))

# # (5) Squared Error ------------------------------------------------
print("squared error of the model, SE:")
SE = t(r) %*% r
print(c(SE,sqrt(SE)))

# # (6) Root mean Squared Error, RMSE --------------------------------
print("RMSE:")
RMSE = sqrt(SE/(k+1))
print(c(RMSE))
 
# # r^2  -----------------------------------------------------------
SE0 = sum((y-mean(y))^2)
rSquare = (SE0 - SE)/SE0
rSquareAdj = 1 -(SE/(length(x)-(k+1)))/(SE0/(length(x)-1))
print(c(rSquare,rSquareAdj)) #rSquaredAdj is ra^2
# 
# # Statistical Modeling -------------------------------------------
mod = lm(y ~ poly(x,k))
print(summary(mod))