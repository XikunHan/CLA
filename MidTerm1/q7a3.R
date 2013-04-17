########## Problem 7 (a) ###############
#Statement 3
# This function tests positive definite property
# using the eigenvalues
# input M is a matrix
# return true if M is positive definite
isPosD = function(M){
  eigns = eigen(M)$values #gets all the eigen values
  for(i in (i:length(eigns))){
    if(eigns[i] <= 0){
      return (FALSE)
    }
  }
  return (TRUE)
}

# This function returns the position where matrix m stop being
# positive definite
# input a and b are the search intervals
searchPosD = function(a=0, b=1, tol = 10^-6){
   #tolerance for when to stop
  while (abs(a-b) > tol){
    mid = (a+b)/2
    m = M(tau=mid)
    if(isPosD(m)){
      a = mid
    } else {
      b = mid
    }
    #print(a)
    #print(b)
    print("Seaching")
    print(c(a,b))
  }
  return ((a+b)/2)
}


#funciton for statement 3
st3 = function(tau=0.001,n=500){
  m = M(tau,n)  
  if(isPosD(m)){
    print("Positive Definite")
    return(TRUE)
  }
  else{
    print("Not Positive Definite")
    return(FALSE)
  }
}

n = 500
taus = 0.1^seq(1,10)
print("Statement 3, Positive Definite Property")
taus = 0.1^seq(5,10)
print("For tau close to 0")
 for (t in taus){
   print("tau=")
   print(t)
   st3(t,n)
}

print("When tau increases")
taus = seq(0.1,0.9,by=0.1)
for (t in taus){
  print("tau=")
  print(t)
  st3(t,n)
}

print("Finding tau where the matrix change stops being positive definite")
#we know the answer is between 0.1 and 0.2
print(searchPosD(0.1,0.2, tol=10^-6))



