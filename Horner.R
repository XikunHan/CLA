Horner = function(coeffs, x) {
  y = coeffs[length(coeffs)]
  for (i in (length(coeffs)-1):1){
    y = y*x + coeffs[i]
  }
  return(y)
}