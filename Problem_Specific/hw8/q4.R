## This file contains solution for Problem 4 Homework 8

######################### Setting Up Matrices #####################
b = c(1,1,2,2,3)
A = cbind(c(1,2,3,4,5),c(5,4,3,2,1),c(1,1,0,-1,-1),c(0,1,1,1,0), c(1,0,1,0,2))
B = cbind(c(1,2,3,4,5),c(1,1,1,1,1),c(5,4,3,2,1),c(1,1,0,-1,-1), c(1,0,1,0,2))

######################### svdSolve Function ############################
svdSolve = function(M,b){
  foo = svd(M)
  V = foo$v
  sings = foo$d
  U = foo$u
  pseudo = V %*% diag(1/sings) %*% t(U)
  
  c = t(U) %*% b
  y = c/sings
  x = V %*% y
  bhat = M %*% x
  r = M %*% x  - b
  err = vnorm(r,p=2)
  return(list(pseudo=pseudo, c=c, y=y, x=x, bhat=bhat, r=r, err = err))
}

########################## Calculations ###############################

resultsA = svdSolve(A,b)
print("Pseudo Inverse of A:")
print(resultsA$pseudo)
print("SVD Solve Solution to A:")
print(resultsA$x)

resultsB = svdSolve(B,b)
print("Pseudo Inverse of B:")
print(resultsB$pseudo)
print("SVD Solve Solution to B:")
print(resultsB$x)
print("Error of the solution to B")
print(resultsB$err)
print("t(bhat) for B and b")
print(t(resultsB$bhat))

# Check the condition number
print("Condition Number of A (invertiable):")
print(Cond(A))
print("Condition Number of B (not invertiable):")
print(Cond(B)) 

# For A, compare the inverse of A
print("The inverse of A is the same as its pseudoinverse")
print(solve(A))

# For B, run Least Square using normal equation
print("Condition Number for t(B) %*% B")
print(Cond(t(B) %*% B))

print("RREF of B, notice that the third column is redundant")
print(rref(B))

B = B[,c(1,2,4,5)] #get rid of the redundant column 3

coeffs = solve(t(B)%*%B, t(B)%*%b) # this x bar, normal equation
print("Lease Square solution to B (excluding the third column):")
print(coeffs)

bhat = B %*% coeffs  # the predicted values 
print("t(bhat) for matrix B")
print(t(bhat))
r = b - bhat # r: the residuals 

# Squared Error ------------------------------------------------
print("squared error of the model, SE:")
SE = t(r) %*% r
print(c(SE,sqrt(SE)))

# Root mean Squared Error, RMSE --------------------------------
print("RMSE:")
RMSE = sqrt(SE/length(r))
print(c(RMSE))