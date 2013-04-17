#This function create the tri-diagonla matrix describe in Problem 4
T = function(x,n){
  d = rep(x,n)
  a = rep(x/3,n-1)
  b = rep(x/3,n-1)
  
  t = TriDiag(d,a,b)
  return (t)
}