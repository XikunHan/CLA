#Computer Problem 3.1: 3

############### (a) Interpolator Function ##############
#This is non-vectorized version
InterpolatorNonVec = function(xx,yy,x,method="NewtonDD"){
  if (method=="Vandermonde"){
    n = length(xx)
    V = Vandermonde(xx)
    c = solve(V,yy) # c contains all the coefficients
    p = function(z) { 
      c %*% z^(0:(length(c)-1))
    }
    return(p(x))
  } else {
    c = NewtonDD(xx,yy); # get the coefficients
    p = function(z) { # this is the new function we have
      Horner(c,z,xx) 
    }
    return (p(x)) 
  }
}

#Vectorized version
Interpolator = function(xx,yy,x,method="NewtonDD"){
  if (length(x) == 1){ #Use the non-vectorized version
    return(InterpolatorNonVec(xx,yy,x,method))  
  } else {
    results = rep(0,length(x)) #results is a collection of result computed by non-vectorized
    for (i in 1:length(x)){
      results[i] = InterpolatorNonVec(xx,yy,x[i],method)
    }
    return(results)
  }
}

#xx = c(-8,1,2,3,4)
#yy = c(0,10,6,4,10)

############## (b) Solve problem 13 section 3.1 ############
xx = seq(-5,5)
yy = rep(5,10)
yy = c(yy, 42)
x = 6
print("Result From Vandermonde:")
print(Interpolator(xx,yy,x,method="Vandermonde"))
print("Result From NewtonDD")
print(Interpolator(xx,yy,x,method="NewtonDD"))
#both produce the same result

############## (c) Is your function vectorized ############
## Yes!
x = c(6,7,8)
print("Vectorized calculation for x=")
print(x)
print(Interpolator(xx,yy,x,method="Vandermonde"))
print(Interpolator(xx,yy,x,method="NewtonDD"))

############# (d) Two Different Answers ##########
# When the length of xx is long, the Varndermonde matrix would
# become ill-conditioned
# > v = Vandermonde(seq(1,100))
# > Cond(v)
# [1] 1.159116e+16
print("When the length of xx is long, the Vandermonde matrix become ill-conditioned")
xx = seq(1,100)
yy = seq(1,100)
x = 6
print("Result From NewtonDD")
print(Interpolator(xx,yy,x,method="NewtonDD"))
print("Error From Vandermonde:")
print(Interpolator(xx,yy,x,method="Vandermonde"))