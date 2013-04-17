# Example 2.25 in the book

n = 10000

#We should put in 1/2 in first and others can overwrite this
A = spMatrix(n,n,i=n:1,j=1:n,rep(1/2,n))
#Put 3 in
diag(A) = 3
#Noted the use of cbind()
A[cbind(2:n,1:(n-1))] = -1
A[cbind(1:(n-1),2:n)] = -1

b = c(2.5,rep(1.5,(n-4)/2),1,1,rep(1.5,(n-4)/2),2.5)

#B is the same matrix but not sparse
B = matrix(0, nrow=n, ncol=n)
B[cbind(n:1,1:n)] = 1/2
diag(B) = 3
B[cbind(2:n,1:(n-1))] = -1
B[cbind(1:(n-1),2:n)] = -1

#print(system.time(solve(A,b)))
#print(system.time(solve(B,b)))