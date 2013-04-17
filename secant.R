secant = function(f, a, b, tol = 1e-05, maxiters = 12) { #a and b are our first guess
  history = rep(NA, maxiters)
  history[1] = a
  history[2] = b
  for (k in 3:maxiters) {
    x1 = history[k-2]
    x2 = history[k-1]
    newx = x2 - (f(x2)*(x2-x1))/(f(x2)-f(x1)) #formula for intercept on x-axis
    history[k] = newx
    change = newx - history[k-1]
    if (abs(change) < tol * max(abs(history[k - 1]), tol)) break
  }
  return(list(root = newx, history = history[!is.na(history)]))
}

# secant implements the secant method for root finding
# section 1.5.1 in Sauerg