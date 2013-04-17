hour = (1:10)
concentration = c(6.2,9.5,12.3,13.9,14.6,13.5,13.3,12.7,12.4,11.9)

plot(hour,concentration,pch=19,col='black',
     xlim=c(0,16),ylim=c(0,20),ylab="concentration (ng/ml)",xlab='time (hours)')
grid()

A = matrix(nrow=length(hour),ncol=2) 
A[,1] = rep(1,length(hour))
A[,2] = hour
print("Matrix A is ")
print(A)

b = log(concentration) - log(hour)

print("Coefficients are ")
coeffs = solve(t(A)%*%A, t(A)%*%b) # this x bar, normal equation
print(t(coeffs))

c = exp(1)^coeffs[1]
k = coeffs[2]
print("C is ")
print(c)
print("K is ")
print(k)

q = function(t){
  c*t*exp(1)^(k*t)
}

xx = seq(hour[1]-1,hour[length(hour)]+1,len=1000)
lines(xx,q(xx),col='blue')
