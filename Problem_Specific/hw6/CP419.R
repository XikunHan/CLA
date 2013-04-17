# HW6. Problem 5. Modeled after Computer Problem 4.1.9 in the book

x = seq(2,4,len=11)
d = 5
y = rep(1,11)
for (i in 1:d) y = y + x^i
plot(x,y,pch=20,col='blue',xlim=c(1.5,4.5))
grid()
fitting(k=d, x=x, y=y)

d = 6
for (i in 1:d) y = y + x^i
#fitting(k=d, x=x, y=y) #cannot run!
A = matrix(nrow=length(x),ncol=(d+1)) #be aware of the redundancy
for (i in 0:d)  A[,i+1] = x^(i) # set up A
print("Condition number for A when d=6")
print(Cond(t(A) %*% A))

d = 8
for (i in 1:d) y = y + x^i
#fitting(k=d, x=x, y=y) #cannot run!
A = matrix(nrow=length(x),ncol=(d+1)) #be aware of the redundancy
for (i in 0:d)  A[,i+1] = x^(i) # set up A
print("Condition number for A when d=8")
print(Cond(t(A) %*% A))