## This program contains the answer to Problem 2 Homework 7
######################### Diagonalization Function #########################
diagonalize = function(A){
  result = eigen(A)
  V = result$vectors
  val = result$values
  D = diag(val)
  return(list(V=V, D=D))
}

####################### Diagonalize A and Verify #######################
A = cbind(c(5,4,3,2,1),c(4,4,3,2,1),c(3,3,3,2,1),c(2,2,2,2,1),c(1,1,1,1,1))
result = diagonalize(A) # A is symmetric 
print(result)
Q = result$V # orthogonal because A is symmetric and lambda are disinct
D = result$D
newA = Q %*% D %*% t(Q)  # t(Q) = solve(Q)
print("Verify Q is Orthogonal")
print(t(Q)%*%Q)
print("Original A:")
print(A)
print("newA = QDt(Q)")
print(newA)








