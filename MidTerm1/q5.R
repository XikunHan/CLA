n = 15

f = function(x){
  return (max(eigen(T(x,n))$value) - 2013)
}

result = bisect(f, c(1,10000), tol=.5*10^-5)
print("The solution for x is")
print(result)
#print(uniroot(f, c(1,10000), tol=.5*10^-5))