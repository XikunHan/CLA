########## Problem 7 (a) ###############
#Statement 2
st2 = function(tau=0.001,n=500){
  m = M(tau=tau,n=n)
  cond = Cond(m)
  print(cond)
}

n = 500
taus = 0.1^seq(1,10)
print("Statement 2, Condition of the matrix")
print("For tau close to 0")
taus = 0.1^seq(5,10)
for (t in taus){
  print("tau=")
  print(t)
  st2(t,n)
}
print("When tau increases")
taus = seq(0.1,0.9,by=0.1)
for (t in taus){
  print("tau=")
  print(t)
  st2(t,n)
}