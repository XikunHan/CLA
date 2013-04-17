############ Creating Adjacency Matrix A #############################
W = read.csv("/Users/zixiao/Dropbox/College Career/Sophomore Year/Math 365/RStudio/TR2/links1000.csv");
print(head(W)) #read in link information and print a preview
from=W$from 
to=W$to
sites = sort(union(unique(to),unique(from))) #sites is a collection of all pages in links1000.csv
n=length(sites) #This counts the number of pages

# The following functions convert between the site labels and the matrix index
# site labels are used by links1000.csv
# matrix index is ths index of sites
SiteToIndex = function(s) {match(s,sites)}
IndexToSite = function(s) {sites[s]}

ii = rep(0,length(to))  #position vector indicating the row
jj = rep(0,length(from)) #position vector indicating the collumn
#find all "from" pages and put their indices for sites collection into the position vector ii
for (i in 1:length(from)) ii[i] = SiteToIndex(from[i]) 
#find all "to" pages, which corresponds to "from" pages, and put their indices for sites collection into jj
for (i in 1:length(to)) jj[i] = SiteToIndex(to[i])
xx = rep(1,length(jj)) #this means all nonzero vector is 1
#A is the adjacency matrix in which entry (i,j) is 1 if and only if there is a link from page i to page j
A = spMatrix(nrow=n,ncol=n,i=ii,j=jj,x=xx)

########### Computing p Katz Status Index #######################
KatzWiki = function(a=0.001, titles=FALSE){
  # a is attentuation factor
  # title=TRUE prints the title of the Wikipedia site.
  n = nrow(A) # A is the adjacency matrix
  transA = t(A)
  one = rep(1,n)
  d = transA %*% one
  I = diag(n)
  
  B = (a^(-1))*I-transA  
  
  x = rep(1, n) #x is the all 1's vector for the initial guess of Jacobi Iteration
  p = jacobi(A=B,b=d,x=x) # Katz Status index for the Wikipedia Sites
  
  # Sort the result in decreasing order
  # results or the matrix index of p
  index = order(p, decreasing=TRUE) 
  
  # print the name of the top sites
  if(titles){
    s = 10 #the number of top sites we wish to print
    print("The name of the Top Sites are")
    for (i in 1:s){
      print(tfile[IndexToSite(index[i])])
    }
  }
  return(list(scores=p,index=index))
}

########## Find the upper bound of attenuation factor alpha ######

upper = convergeAlpha(a=0,b=1,M=A)

print("The upper bound of attenuation factor alpha is ")
print(upper)

########## Try different attenuation factor alpha, based on the upper bound #######
s = 10 # the number of sites we need to print

alpha = seq(0.00001,upper,length=5)

title = TRUE #specify whether to print the titles of the sites

if(title){
  con <- file("/Users/zixiao/Dropbox/College Career/Sophomore Year/Math 365/RStudio/TR2/titles-sorted.txt", "rt") 
  tfile = readLines(con)
}

for (i in alpha) {
  print("alpha = ")
  print(i)
  p = KatzWiki(a=i,titles=title) #this is the Katz index
  print("KatzWiki Status Index are")
  for (id in p$index[1:s]){
     score = p$scores[id]
     print(score)
  }
}

if(title){
  close(con)
}


