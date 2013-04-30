## This file contains solution for Problem 3 Homework 8

################################ Setting up matrix #################################
#library("foreign", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

cereal = read.dta("http://statistics.ats.ucla.edu/stat/data/cerealnut.dta")
print(cereal)
A =  as.matrix(cereal[,2:9])
print(A)

############################### Dimension Reduction ##############################
n = ncol(A)

for(i in (1:n)){ # find resduials 
  A[,i] = A[,i] - mean(A[,i])
}

C = A %*% t(A) # covariance matrix
foo = svd(C)
sings = foo$d #noted only the first and second singular values matter
V = foo$v
plot(sings)
x = V[,1]
y = V[,2]
plot(x,y,pch=20,col='red')
text(x,y,labels=cereal[,1])

