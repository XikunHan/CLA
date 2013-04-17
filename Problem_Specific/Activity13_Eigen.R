A = cbind(c(-17,-30,-3,-6), c(14.5,22.5,-2.0,0.5),c(-16,-26,0,-2),c(3.5,8.5,5,6.5))
print(A)

########################## (1) PI #############################
## Write a loop that performs power iteration to find the dominant 
## eigenvector and eigenvalue of A. See how many steps it takes to 
## get to the point where consecutive iterations of the eigenvector
## are within .000005 in the infinity norm. Have it print the eigenvector
## and eigenvalue

PI = function(A, x = seq(1,nrow(A)), tol,maxiter){
  step = 0; #this count the step
  for (i in 1:maxiter ){
    step = step+1
    xold = x
    x = A %*% x
    x = x/vnorm(x) #normalize
    if(vnorm((xold-x),'I') <tol)
      break
  }
  x = x/vnorm(x)
  lam = (t(x)%*% A%*% x/(t(x)%*%x))[1] 
  return(list(value=lam, vector=x, steps=step))
}

print("Running PI")
tol = 0.000005
resultsPI = PI(A, tol=tol,maxiter=100)
print(resultsPI)

####################### (2) IPI ##############################
## Do the same but now use IPI to find the minimum eigenvector
IPI = function(A, x = seq(1,nrow(A)), tol,maxiter){
  step = 0; #this count the step
  for (i in 1:maxiter ){
    step = step+1
    xold = x
    x = solve(A,x)
    x = x/vnorm(x)
    if(vnorm((xold-x),'I') <tol)
      break
  }
  x = x/vnorm(x)
  lam = (t(x)%*% A%*% x/(t(x)%*%x))[1] 
  return(list(value=lam, vector=x, steps=step))
}

print("Running IPI")
tol = 0.000005
resultsIPI = IPI(A, tol=tol,maxiter=100)
print(resultsIPI)

###################### (3) SIPI ###############################
## Now add in a shift to find the other two eigenvectors
SIPI = function(A, x = seq(1,nrow(A)),s=0,tol,maxiter){
  step = 0; #this count the step
  As = A -s*diag(nrow(A))
  for (i in 1:maxiter ){
    step = step+1
    xold = x
    x = solve(As,x)
    x = x/vnorm(x)
    if(vnorm((xold-x),'I') <tol)
      break
  }
  x = x/vnorm(x)
  lam = (t(x)%*% A%*% x/(t(x)%*%x))[1] 
  return(list(value=lam, vector=x, steps=step))
}

print("Running SIPI 1")
tol = 0.000005
s = (resultsPI$value+resultsIPI$value)/2
resultsSIPI = SIPI(A, tol=tol, s=s,maxiter=100)
print(resultsSIPI)

print("Running SIPI 2")
resultsSIPI2 = SIPI(A, tol=tol, s=-1,maxiter=100)
print(resultsSIPI2)

#################### (4) RQI ############################
## Write a loop to perform Rayleigh Quotient Iteration. 
## Have it stop when the rank of the matri A - lambda I
## is less than n.
RQI = function(A, x = seq(1, nrow(A)),tol,maxiter){
  x = x/vnorm(x)
  s = ((t(x) %*% A %*% x)/(t(x)%*%x))[1]
  step = 0; #this count the step
  n = nrow(A)
  As = A -s*diag(n)
  for (i in 1:maxiter ){
    step = step+1
    xold = x
    x = solve(As,x)
    x = x/vnorm(x)
    if(vnorm((xold-x),'I') <tol || rankMatrix(As)[1]<n)
      break
    s = ((t(x) %*% A %*% x)/(t(x)%*%x))[1]
    As = A -s*diag(n)
  }
  return(list(value=s, vector=x, steps=step))
}

print("Running RQI")
resultsRQI = RQI(A, tol=tol,maxiter=100)
print(resultsRQI)


