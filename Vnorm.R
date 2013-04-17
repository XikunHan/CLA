vnorm = function(v,p=2) { 
  if ( p =="I") {
    return(max(abs(v)))
  }
  else {
    return(sum(abs(v)^p)^(1/p))
  }
}