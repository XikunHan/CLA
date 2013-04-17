TriDiag = function(d,a,b,sparse = FALSE){
  # d is the entries on the main diagonal
  # a is the entries below diagonal
  # b is the entries above diagonal
  # n is the size of the matrix
  n = length(d)
  if(sparse){
    #Noted this requires matrix package
    A = Matrix(0,nrow=n,ncol=n,sparse = TRUE) 
  }else{
    A = matrix(0,nrow=n,ncol=n) 
  }
  
  diag(A) = d #filling diagonal
  A[cbind(2:n,1:(n-1))] = a #filling lower
  A[cbind(1:(n-1),2:n)] = b #filling upper
  return (A)
} 

# n = 10000
# d = rep(1, n)
# a = rep(2, n-1)
# b = rep(3, n-1)
# A = TriDiag(d,a,b)
# As = TriDiag(d,a,b,TRUE)
# print("n = ")
# print(n)
# print("spare/dense ratio")
# print(object.size(As)/object.size(A))
