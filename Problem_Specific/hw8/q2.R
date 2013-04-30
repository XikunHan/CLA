## This R program provides solutions to Homework 8, Question 2

library(jpeg) # use the library

######################### Setting up Image ######################
where = "http://www.math.pitt.edu/~sussmanm/2071/lab09/mars.jpg"
# just matrix of grey scale value
# 500 x 500
img = readJPEG(readBin(where,"raw",1e6))

#img = readJPEG("< the location of the file here> ")
imPlot = function(img,...) {
  plot(1:2, type='n',xlab=" ",ylab= " ",...)
  rasterImage(img, 1.0, 1.0, 2.0, 2.0)
}

imPlot(img,main="Mars Image") #plot original Image

######################## Try SVD Approximation ######################
# plot(svd(img)$d) #plot of singular values
# 
# M = SVDApprox(img,100) #SVD Approximation
# # noted grey scale is 0 to 1, so we need to truncate
# M[M>1] = 1 #fix grey scale > 1
# M[M<0] = 0 #fix grey scale > 0
# 
# imPlot(M,main="Mars Image Approx, k=100")
# 
# M2 = SVDApprox(img,50) #SVD Approximation
# M2[M2>1] = 1 #fix grey scale > 1
# M2[M2<0] = 0 #fix grey scale > 0
# 
# imPlot(M2,main="Mars Image Approx, k=50")
# 
# M3 = SVDApprox(img,10) #SVD Approximation
# M3[M3>1] = 1 #fix grey scale > 1
# M3[M3<0] = 0 #fix grey scale > 0
# 
# imPlot(M3,main="Mars Image Approx, k=10")

###################### SVDImageCompression Function #####################
SVDImageCompression = function(img, k = floor(min(nrow(img),ncol(img))/2)){
  M = SVDApprox(A=img, k=k)
  M[M>1] = 1 #fix grey scale > 1
  M[M<0] = 0 #fix grey scale > 0
  
  title = paste("Mars Image at k=", k, "Singluar Values")
  imPlot(M,main=title)
  
  m = dim(img)[1]
  n = dim(img)[2]
  floatImg = m*n # floating point numbers for original image
  floatM = m*k + k + k*n # floating point numbers for compressed image M
  ratio = floatM/floatImg
  
  return(list(usage=floatM, ratio=ratio, compressed = M))
}

###################### Using SVDImageCompression Function ##############
k = c(5,10,25,50,100)
#k = seq(60,70)
for(i in k){
  results = SVDImageCompression(img, k=i)
  outk = paste("k =",i)
  print(outk)
  outr = paste("compression ratio =",results$ratio)
  print(outr)
}

##################### Compare 1/4 compression to Halverson ###########
SVDImageCompression(img, k=63) #compression ratio = 0.252252

## Halverson construct the new m/2 x n/2 matrix
m = dim(img)[1]
n = dim(img)[2]
H = img[seq(1,m,length=m/2),seq(1,n, length=n/2)]
imPlot(H,main="Halverson Compression at ratio 1/4")




