#########################################################################

# vector norm
vnorm = function(v,p=2) { 
  if ( p =="I") {
    return(max(abs(v)))
  }
  else {
    return(sum(abs(v)^p)^(1/p))
  }
}

#########################################################################

# Derivative
D = function(f,delta=.000001){ 
  function(x){(f(x+delta) - f(x-delta))/(2*delta)}
}

#########################################################################

fpi = function(g, x, tol = 1e-05, maxiters = 10) {
  history = rep(NA, maxiters)
  history[1] = x
  for (k in 2:maxiters) {
    newx = g(history[k-1])
    history[k] = newx
    change = newx - history[k-1]
    if (abs(change) < tol * max(abs(history[k - 1]), tol)) break
  }
  return(list(root = newx, history = history[!is.na(history)]))
}

# fpi implements the fixed-point iteration function discussed
# in Section 1.2 of Sauer.
# It successively iterates: x_1 = x and  x_{n+1} = g(x_n)
# until there is relatively little change or maxiters is reached
# It uses the hybrid stopping criterion mentioned in 1.2.4.
# It also illustrates the use of list for returning multiple
# outputs from a function. In this case we return both the root 
# and the successive iterations that got us there.
#########################################################################

secant = function(f, a, b, tol = 1e-05, maxiters = 12) {
  history = rep(NA, maxiters)
  history[1] = a
  history[2] = b
  for (k in 3:maxiters) {
    x1 = history[k-2]
    x2 = history[k-1]
    newx = x2 - (f(x2)*(x2-x1))/(f(x2)-f(x1))
    history[k] = newx
    change = newx - history[k-1]
    if (abs(change) < tol * max(abs(history[k - 1]), tol)) break
  }
  return(list(root = newx, history = history[!is.na(history)]))
}

# secant implements the secant method for root finding
# section 1.5.1 in Sauer

#########################################################################

eliminate = function(A,tol=10^-8) {
  n = nrow(A)
  for ( j in 1:(n-1) ) {
    pivot = A[j,j]
    if (abs(pivot) < tol) stop('zero pivot encountered')
    for ( i in (j+1):n ) {
      A[i,] = A[i,] - A[i,j]/pivot * A[j,]
    }
  }
  return(A)
}


MySolve = function(A,b,tol=10^-8) {
  n = nrow(A)
  A = cbind(A,b)     # append b to the right of A
  A = eliminate(A)  # Gaussian elimination below the main diagonal
  b = A[,n+1]        # Get the row reduced version of b back
  x = rep(0,n)       # pre-allocate a vector x with 0s in it.
  x[n] = b[n]/A[n,n] # Fill in the nth value of A
  for (j in (n-1):1 ) {
    back = 0                  #  compute the back substitution part with a for loop
    for (k in (j+1):n ) {
      back = back + A[j,k]*x[k]
    }
    x[j] = (b[j] - back)/A[j,j]
  }
  return(x)
}

# The following version is vectorized and does not
# require an inner for loop to compute the back substitution

MySolveV = function(A,b,tol=10^-8) {
  n = nrow(A)
  A = cbind(A,b)     # append b to the right of A
  A = eliminate(A)  # Gaussian elimination below the main diagonal
  b = A[,n+1]        # Get the row reduced version of b back
  x = rep(0,n)       # pre-allocate a vector x with 0s in it.
  x[n] = b[n]/A[n,n] # Fill in the nth value of A
  for (j in (n-1):1 ) {
    back = A[j,((j+1):n)] %*% x[(j+1):n]   # compute the back sub part with a dot product
    x[j] = (b[j] - back)/A[j,j]
  }
  return(x)
}

#########################################################################

# Solves Ax = b iteratively using the Jacobi Method
# m is the maximum number of iterations: default m = 25
# p is the p value of the matrix norm
# tol is the stopping tolerance (using the relative residual norm on the backward error)
jacobi = function(A,b,m=25,x = rep(0,n),p=2,tol=0.5*10^(-6),history=FALSE) {
  n = length(b)
  if (history) {
    hist = matrix(NA,nrow=length(b),ncol=(m+1))
    hist[,1] = x
  }
  d = diag(A)
  R = A
  R[cbind((1:n),(1:n))] = 0  # allows for r to be sparse  
  steps=0
  for (j in 1:m) {
    x = (b - R %*% x)/d 
    steps = steps+1
    if (history) {hist[,j+1] = x}
    if (vnorm(b-A%*%x,p) <= vnorm(b,p)*tol) break 
  }
  if (history) return(list(x=x,iterations=steps,history = hist[,1:(steps+1)]))
  else return(list(x=x,steps=steps))
}


#########################################################################

Cond = function(A,p=2) {
  if (p == 2) {  # by default use the 2-norm
    s = svd(A)$d
    s = s[s>0]
    return(max(s)/min(s))
  }
  if (p == 1) {  # use the 1 norm
    Ainv = solve(A)
    return(max(colSums(abs(A)))*max(colSums(abs(Ainv))))
  }
  if (p == 'I') {   # use the infinity norm
    Ainv = solve(A)
    return(max(rowSums(abs(A)))*max(rowSums(abs(Ainv))))
  }
}
#########################################################################

# vectorized tridiagonal method
# Matrix package needs to be on for the sparse version

TriDiag = function(d,l,u, sparse=FALSE) {
  n = length(d)
  if ( length(l) != (n-1) || length(u) != (n-1) ) 
    stop('vectors not of correct tridiagonal length')
  if (sparse) {
    D = Matrix(0,nrow=n,ncol=n,sparse=TRUE)
    diag(D) = d
  }
  else  D = diag(d)
  D[cbind((1:(n-1)),(2:n))] = u
  D[cbind((2:n),(1:(n-1)))] = l
  return(D)
}

# straight-forward tridiagonal method with a single for-loop

TriDiagL = function(d,l,u) {
  n = length(d)
  if ( length(l) != (n-1) || length(u) != (n-1) ) 
    stop('vectors not of correct tridiagonal length')
  D = diag(d)
  for (i in 1:(n-1)) {
    D[i,i+1] = u[i]
    D[i+1,i] = l[i]
  }
  return(D)
}

#########################################################################

ConjGrad = function(A,b,x = rep(0,length(b)),tol=2*10^-16,m = length(b),history=FALSE) {
  # Conjugate Gradient Method
  # Inputs: symm pos def matrix A, rhs b, number of steps n, 
  # x is the initial guess, 
  # tol is a stopping condition on the size of the residual
  # history gives a full history
  # Output: solution x to Ax=b
  # 
  n = length(b)
  r = b - A %*% x
  d = r
  if (history) {
    hist = matrix(NA,nrow=n,ncol=m+1)
    hist[,1] = x
  }
  for (i in 1:m ) {
    steps = i
    if (max(abs(r)) < tol) break
    alpha = (t(r) %*% r)/(t(d) %*% A %*% d)  # step length
    x = x + alpha[1,1] * d                  # take step 
    if (history) {hist[,i+1] = x[,1]}
    rold = r
    r = rold - alpha[1,1] * A %*% d         # new residual
    beta = (t(r) %*% r)/(t(rold) %*% rold)   # improvement this step
    d = r + beta[1,1]*d
  }
  if (history) {return(list(x=x,history=hist))}
  else {return(list(x=x,steps=steps))}
}
