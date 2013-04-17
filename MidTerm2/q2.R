##This file contain solution for Exam 2 Question 2

############################ Frank Matrix Function ###################
## This function create n by n frank matrix
frankM = function(n){
  F = matrix(0,nrow=n, ncol=n)
  diag(F) = seq(n,1) #fill diagonal
  F[cbind(1:(n-1),2:n)] = seq(n-1,1) #fill upper
  #fill lower
  for(i in 2:n)
    F[cbind(i:n,1:(n-i+1))] = seq((n-i+1),1)
  return(F)
}

############################ Problem (a) ############################
n = 5
F = frankM(n=n)
q = qr(F)
Q = qr.Q(q)
R = qr.R(q)

print("F=")
print(F)
print("QR =")
print(Q%*%R)

########################### Problem (b) ############################
# This function tests frank matrix with different n
testFrankM = function(n){
  F = frankM(n=n)
  x = seq(1,n)
  b = F %*% x
  
  #print(c(n,Cond(F)))
  
  xx1 = solve(F,b) #R's built in solver
  xx2 = MySolve(F,b) # MySolve using Gaussian-Elim
  
  # QR Decomposition
  q = qr(F)
  Q = qr.Q(q)
  R = qr.R(q)
  qy = t(Q) %*% b
  xx3 = solve(R,qy)
  xx4 = MySolve(R,qy)
  if(n==17)
    print(R)
  print(Cond(R))
  
  # Find Forward Error
  ferr1 = vnorm(xx1-x,p='I')
  ferr2 = vnorm(xx2-x,p='I')
  ferr3 = vnorm(xx3-x,p='I')
  ferr4 = vnorm(xx4-x,p='I')
  
  #print(c(n,ferr1,ferr2,ferr3,ferr4))
  print(c(n,0,ferr2,ferr3,ferr4))
  #print(c(n,0,ferr2,0,ferr4))
}

for (i in 6:20){
  testFrankM(n=i)
}