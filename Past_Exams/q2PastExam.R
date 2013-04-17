q2PastExam = function(A,b){
  n = nrow(A)
  x = cbind(c(1,1,1))
  xtemp = x
  x = A %*% x + b
  
  m=1000
  tol = 10^-5
  
  count = 1
  p = 'I'
  
  while(count<m && vnorm((x-xtemp),p) > tol){ 
    count=count+1
    xtemp = x
    x = A %*% x + b  
    #print(x)
  }
  print(count)
  
  return(x)
}