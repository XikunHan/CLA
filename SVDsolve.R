# This problem won't solve with R's solve

x = seq(1,1.1,len=20)
n = 6
A = matrix(nrow=length(x),ncol=n)
for ( i in 1:n )  A[,i] = x^(i-1)

xbar = 1:n
b = A %*% xbar


# print(Cond(t(A) %*% A))
# soln = solve(t(A) %*% A, t(A) %*% b)
# print(soln)
# print(Cond(t(A) %*% A))

# SVD solve to the rescue
foo = svd(A)
sings = foo$d
print(sings)
V = foo$v
U = foo$u
c = t(U) %*% b
y = c/sings
x = V %*% y
print(x)
