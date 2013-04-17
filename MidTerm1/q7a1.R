########## Problem 7 (a) ###############
#Statement 1
st1 = function(tau=0.001,n=500){
  m = M(tau=tau,n=n) #n is 500 by default
  count = 0
  for(i in (1:n)){
    for(j in (1:n)){
      if(m[i,j]!=0)
        count = count+1
    }
  }
  density = count/(n*n)
  print(density)
}

n = 500
taus = 0.1^seq(1,10)
print("Statement 1, density of nonzero entries:")
for (t in taus){
  print("tau=")
  print(t)
  st1(t,n)
}