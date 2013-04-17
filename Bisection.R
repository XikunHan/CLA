# bisect = function(f, interval, tol = .5*10^-10) { #tol is tolerance, 10 digits of accuracy
#     a = interval[1] #noted index start with 1
#     b = interval[2]
#     
#     fa = f(a) #where f is a function we want to find root
#     fb = f(b)
#     
#     if (sign(fa)*sign(fb) >= 0) stop("f(a)f(b)<0 not satisfied") 
#     #sign returns 1, 0, -1; no root
#     
#     while((b-a)/2 > tol) {
#       c = (a+b)/2 #get the mid point
#       fc = f(c)
#       
#       if(sign(fa)*sign(fc) < 0){
#         b = c
#         fb = fc
#       }
#       else{
#         a = c
#         fa = fc
#       }
#       
#     }
#     
#     return ((a+b)/2)
#   }
bisect = function(f,interval,tol=0.5*10^-10,verbose=FALSE){
  a = interval[1]
  b = interval[2]
  fa = f(a)
  fb = f(b)
  j = 0                                # counter for verbose printing
  if (sign(fa)*sign(fb) >= 0 ) 
    stop("f(a)f(b)<0 not satisfied")
  while ((b-a)/2 > tol) {
    c = (a + b)/2
    fc = f(c)
    j = j+1
    if (verbose==TRUE) {               # show values at each step
      print(j) 
      print(c(a,c,b,b-a))
    }
    if (fc == 0)  break
    if (sign(fc)*sign(fa) < 0) {
      b = c
      fb = fc
    }
    else {
      a = c
      fa = fc
    }
  }
  return((a+b)/2) 
}