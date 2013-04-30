## This R program provides solutions to Homework 8, Problem 1

########################### Setting Up Problem ####################
b = c(15,12,8,5,7)
A = cbind(c(1,1,0,1,1),c(-1,1,0,1,1),c(2,4,4,2,1))

########################### (1) reduced SVD #######################
foo = svd(A)
sings = foo$d
print("Singular Values: ")
print(sings)
V = foo$v
U = foo$u
c = t(U) %*% b
y = c/sings
x = V %*% y
bhat = A %*% x
r = A %*% x - b
err = vnorm(r,p=2)
print("c:")
print(c)
print("y:")
print(y)
print("Solution x:")
print(x)
print("bhat:")
print(bhat)
print("|r|:")
print(err)

######################### (2) full SVD ###########################
foo = svd(A,nu=nrow(A))
sings = foo$d
print("Singular Values: ")
print(sings)
V = foo$v
U = foo$u
c = t(U) %*% b
y = c[1:length(sings)]/sings
x = V %*% y
bhat = A %*% x
r = A %*% x - b
# Here, we take advantage of full SVD
err = vnorm(c[(length(sings)+1):length(c)])
print("c:")
print(c)
print("y:")
print(y)
print("Solution x:")
print(x)
print("bhat:")
print(bhat)
print("|r|:")
print(err)