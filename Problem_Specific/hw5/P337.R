maxerror = function(n){
  (((exp(1)-1)/2)^n)/(n*2^(n-1))
}

for(i in 1:15){
  print(i)
  print(maxerror(i))
}
