solveCP2_3_1 = function(n) {
  print(n)
  
  #Construct matrix 
  A = matrix(0, nrow=n, ncol=n)
  for (i in 1:n){
    for (j in 1:n){
      A[i,j] = 5/(i+2*j-1)
    }
  }
  
  x = cbind(rep(1,n))   #Construct x
  b = A %*% x #Construct b

  xc = solve(A,b) #Find solution xc
  
  fe = abs(x-xc) #Forward Error
  #print("Forward error: ")
  #print(fe)
  
  #infinity norm of the forward error 
  nfe = vnorm(fe, 'I')
  print("Infinity norm of the forward error: ")
  print(nfe)
  
  #relative forward error
  rfe = nfe/vnorm(x, 'I')
  print("relative forward error: ")
  print(rfe)
  
  #relative backward error
  be = b - (A %*% xc)
  rbe = vnorm(be, 'I')/vnorm(b, 'I')
  print("relative backward error: ")
  print(rbe)
  
  #find the error magnification factor
  mag = rfe/rbe
  print("Error Magnification Factor")
  print(mag)
  
  #Condition Number
  cond = Cond(A,'I')
  print("Condition Number: ")
  print(cond)
}