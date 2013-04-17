testDelta = function(){
  #range = rep(0.1,10)^seq(1,10,by=1)
  range = rep(10,91)^seq(-1,-10, by=-0.1)
  errs = rep(0,length=10)
  
  f = function(x) {sin(x)}
  
  for (i in 1:91) {
    df = D(f,range[i])
    errs[i] = abs(-1-df(pi))
  }
  
  plot(x=range,y=errs,xlim=c(0.0000001,5*10^-5),ylim=c(0,10^-09),xlab="delta",type="l")  
}