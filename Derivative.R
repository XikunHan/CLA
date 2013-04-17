D = function(f, delta=1e-05){
  g = function(x) {
    (f(x+delta)-f(x-delta))/(2*delta)
  }
  return(g)
}