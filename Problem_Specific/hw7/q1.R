## This program contains the answer to Problem 1 Homework 7

################### Making Matrix A (1)##########################
eigvals = c(10,2,1)
v1 = c(1,2,0)
v2 = c(1,1,2)
v3 = c(-1,0,1) 
v = cbind(v1,v2,v3)
lamb = diag(eigvals)
A = v %*% lamb %*% solve(v)

################### Examine PI (2) ################################
tol = 0.0000000005
resultPI = PI(A=A,tol=tol)
print("Running PI")
print(resultPI)

################### Examine IPI (3) ##############################
resultIPI = IPI(A=A,tol=tol)
print("Running IPI")
print(resultIPI)

################## Making Another A (4) ########################

eigvals2 = c(10,9.9,1) # noted 9.9 is really close to 10, rate 9.9/10
lamb2 = diag(eigvals2)
A = v %*% lamb2 %*% solve(v) #noted 
print("Running PI and IPI for another A")
resultPI2 = PI(A=A,k=1000,tol=tol)
resultIPI2 = IPI(A=A,tol=tol)
print(resultPI2)
print(resultIPI2)
