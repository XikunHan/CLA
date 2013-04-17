NewtonDD = function(x,y,ddtable=FALSE) {
  n = length(x)
  v = matrix(0,nrow=n,ncol=n)
  v[,1] = y
  for (i in 2:n) {
    for (j in 1:(n+1-i)) {
      v[j,i] = (v[j+1,i-1]-v[j,i-1])/(x[j+i-1]-x[j])
    }
  }
  if (ddtable) print(v)
  return(v[1,])
}

Horner = function(coeffs,x,b=rep(0,length(coeffs))) { #b is the shifted position of x
  y = coeffs[length(coeffs)]
  if (length(coeffs) == 1) return(rep(coeffs[1],len=length(x)))
  for (i in (length(coeffs)-1):1) {
    y = y*(x-b[i])+coeffs[i]
  }
  return(y)
}
