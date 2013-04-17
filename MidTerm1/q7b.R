# This function takes a value of tau as input
# and examine the speed of convergence of the
# conjugate gradient method
q7b = function(tau){
  n = 500
  m = M(tau,n) #create matrix m
  
  print("tau = ")
  print(tau)
  
  print("Condition Number: ")
  print(Cond(m))
  
  eigens = eigen(m)$values
  print("Maximum eigenvalue")
  print(max(eigens))
  print("Minimum eigenvalue")
  print(min(eigens))
  
  print("Number of non-zero entry")
  count = 0
  for(i in (1:n)){
    for(j in (1:n)){
      if(m[i,j]!=0)
        count = count+1
    }
  }
  print(count)
  
  b = runif(n,min=-1,max=1) #create b randomly
  
  result = ConjGradQ7(A=m,b=b)
  steps = result$steps
  resids = result$resids
  #plot step n vs. length of residuals
  plot(seq(1,steps),resids, xlim=c(0, steps), ylim=c(0,0.1),xlab='step#n', ylab='length of residual')   
  
  #We can also compare this to Jacobi iteration
  print("Running Jacobi")
  jacobi(m,b,m=500)
  
  print("Time for CG")
  print(system.time(ConjGradQ7(A=m,b=b)))
  print("Time for jacobi")
  print(system.time(jacobi(m,b,m=500)))
}

q7b(tau = 0.00001) #tau really close to zero
q7b(tau = 0.05) #some normal tau
q7b(tau = 0.08)

#a value of tau that is close to the point that the matrix
#stop being positive definite
q7b(tau = 0.115) 

q7b(ta = 0.2) #matrix is not positive definite
