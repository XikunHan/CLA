###### Create the adjacency matrix A for the social network example. #######
## If a turns to b for advice, replace the represented position A[a,b] by 1. 
#ii and jj are the position vectors for the sparse matrix
ii = c(1,2,2,3,4,4,4,5,5,5,6,6,6,7,7,7,8,9,10,10,10,11,12,13,14,14,14)
jj = c(2,1,3,1,1,3,5,1,3,4,1,2,7,1,2,6,4,4,4,8,9,5,5,5,2,6,7)
n = 14
xx = rep(1,length(jj)) #all nonzero vectors are 1
A = spMatrix(nrow=n,ncol=n,i=ii,j=jj,x=xx)

n = nrow(A)

transA = t(A) #get the transpose of A
one = rep(1,n) #this is the vector of all 1's
d = transA %*% one #matrix for degree centrality, how many vertices point in to each node
I = diag(n) #n x n identity matrix

########## Use a specific alpha to demonstrate the steps for Jacobi Iteration #####
#a = 0.4
#x = rep(1, n) #x is the all 1's vector for the initial guess of Jacobi Iteration
#B = (a^(-1))*I-transA #get matrix B using the formula
#p = jacobi(B,d,x,verbose=TRUE) #this is the Katz index

########## We try Jacobi Iteration for different alpha values ######
a=c(0.0001,0.001,0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) #Try different alpha
la=length(a)

x = rep(1, n) #x is the all 1's vector for the initial guess of Jacobi Iteration

for (i in (1:la)) {
  B = (a[i]^(-1))*I-transA #get matrix B using the formula
  p = jacobi(A=B,b=d,x=x) #this is the Katz index
  print("alpha = ")
  print(a[i])
  print(p)
}