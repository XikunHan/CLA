fpi = function(g, x, tol = 1e-8, maxiters = 10) {
  history = rep(NA, maxiters) #vector of NA with length maxiters
  history[1] = x #put first guess to iteration
  for (k in 2:maxiters) {
    newx = g(history[k-1])
    history[k] = newx
    change = newx - history[k-1]
    if (abs(change) < tol * max(abs(history[k - 1]), tol)) break
  }
  return(list(root = newx, history = history[!is.na(history)])) #!is.na(history) eliminate all na values when returning
}

# fpi implements the fixed-point iteration function discussed
# in Section 1.2 of Sauer.
# It successively iterates: x_1 = x and  x_{n+1} = g(x_n)
# until there is relatively little change or maxiters is reached
# It uses the hybrid stopping criterion mentioned in 1.2.4.
# It also illustrates the use of list for returning multiple
# outputs from a function. In this case we return both the root 
# and the successive iterations that got us there.