Newton = function(f, x , tol = 1e-05, maxiters = 10){
  df = D(f, tol) # df is the derivative of the function f
  
  g = function(x){ # g is the function for fpi method
    x-f(x)/df(x)
  }
  
  result = fpi(g, x, tol, maxiters) # run fpi method to get the result
  return(result)
}