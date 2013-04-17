n = 1000000 #define n

#Setting up matrix A and vecotr b
A = spMatrix(n,n,i=n:1,j=1:n,rep(1/2,n))
A[cbind(2:n,1:(n-1))] = -1
A[cbind(1:(n-1),2:n)] = -1
diag(A) = 3

b = c(2.5,rep(1.5,(n-4)/2),1.0,1.0,rep(1.5,(n-4)/2),2.5)

print("n:")
print(n)

print("Running Jacobi Method to find x for Ax=b")
x = jacobi(A,b,m=10000,tol=10^-4)
#print("Result")
#print(x)

print("Running Conjugate Gradient to find x for Ax=b")
x = ConjGrad(A,b,tol=10^-4)
#print("Result")
#print(x)