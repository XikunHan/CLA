# Here we use Interpolation Function
# xx is the x coordinate of sampling points
# yy is the y coordinate of samping points
# x is the input to interpolatng polynormial
# return y outout for x input

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