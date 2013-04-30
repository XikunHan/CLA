A = rbind(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          c(10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          c(9, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          c(20, 10, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          c(19, 12, 11, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          c(31, 21, 23, 11, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          c(14, 14, 11, 21, 13, 29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          c(16, 10, 9, 13, 4, 20, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          c(2, 8, 7, 18, 17, 30, 12, 14, 0, 0, 0, 0, 0, 0, 0, 0),
          c(3, 7, 6, 17, 16, 28, 12, 13, 1, 0, 0, 0, 0, 0, 0, 0),
          c(32, 23, 25, 14, 23, 11, 34, 26, 31, 30, 0, 0, 0, 0, 0, 0),
          c(32, 22, 24, 12, 20, 5, 32, 24, 31, 30, 6, 0, 0, 0, 0, 0),
          c(32, 23, 25, 14, 24, 13, 35, 27, 30, 30, 2, 8, 0, 0, 0, 0),
          c(14, 13, 10, 19, 11, 27, 2, 7, 12, 11, 32, 30, 33, 0, 0, 0),
          c(4, 7, 5, 17, 15, 28, 11, 12, 2, 1, 29, 29, 29, 11, 0, 0),
          c(11, 4, 6, 7, 11, 15, 15, 11, 10, 10, 11, 16, 14, 13, 9, 0))

cities = c("Boston", "Chicago", "Cincinnati", "Denver", 
           "Houston", "Los Angeles", "Miami", "New Orleans", 
           "New York", "Philadelphia", "Portland", "San Francisco", 
           "Seattle", "Tampa", "Washington", "Saint Paul")

A = A + t(A) #this gets the distance between all cities
D = A^2
n = length(cities)
I = diag(n)
J = matrix(1/n, nrow=n, ncol=n)

# noted because D is not quite right, this is just approximation
CCt = (-0.5)*(I-J) %*% D %*% (I-J)

foo = svd(CCt)
sings = foo$d #noted only the first and second singular values matter
V = foo$v
#plot(sings)
x = -sings[1]*V[,1] #noted we need to flip both direcitons
y = -sings[2]*V[,2]
plot(x,y,pch=20,col='red')
text(x,y,labels=cities)