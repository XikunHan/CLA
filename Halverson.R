Halverson = function(coeffs, x){
  result = 0
  for(i in 0:(length(coeffs)-1)){
    result = result+coeffs[i+1]*x^i
  }
  return(result)
}