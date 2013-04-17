LU = function(A, tol=10^-8){ #A is the matrix and tol is tolerance
  n = nrow(A) #number of rows in A
  L = diag(n) #start with diagnoal matrix
  for (j in (1:(n-1))){ #noted we don't clear the last row and index start at 1
    pivot = A[j,j]
    if(abs(pivot)<tol){ #we don't do swap yet, so we stop here
      stop('zero pivot encountered')
    }
    for (i in (j+1):n){
      L[i,j] = A[i,j]/pivot
      A[i,] = A[i, ] - (A[i,j]/pivot)*A[j,]
    }
  }
  return(list(L=L, U=A))
}

PALU = function(A,tol=10^-8) {
  n = nrow(A)
  L = diag(x=1,nrow=n)  # start with L = identity matrix
  P = diag(x=1,nrow=n)  # start with P = identity matrix
  for ( j in 1:(n-1) ) { 
    jcol = abs(A[(j:n),j]) # pick out the part of column j below diagonal
    #m = match(max(jcol),jcol) + j-1  # get the row of the largest element
    m = which.max(jcol) + j-1  # get the row of the largest element
    if (m > j) {    #  swap rows m and j of A
      temp = A[j,]
      A[j,] = A[m,]
      A[m,]= temp
      if (j>1) {    # swap elements of L below diagonal
        temp = L[j,(1:(j-1))]
        L[j,(1:(j-1))] = L[m,(1:(j-1))]
        L[m,(1:(j-1))] = temp
      }
      temp = P[j,]    # swap rows of P
      P[j,] = P[m,]
      P[m,] = temp
    }
    pivot = A[j,j]
    if (abs(pivot) < tol) stop('zero pivot encountered')
    for ( i in (j+1):n ) {
      mult =  A[i,j]/pivot
      A[i,] = A[i,] - mult * A[j,]
      L[i,j] = mult   #  store the multiplier in LA
    }
  }
  return(list(L=L,U=A,P=P))
}