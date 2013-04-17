# Source: U.S. Bureau of Economic Analysis, NIPA Table 2.1.
# Data is given quarterly: use splines to estimate monthly 
#http://chamberlaineconomics.com/2010/01/20/how-economists-convert-quarterly-data-into-monthly-cubic-spline-interpolation/

# Personal Income  ($Billions)

PI=c(8379.6,8506.6 ,8644.2 ,8707.3 ,8859.0 ,8881.2 ,8880.6)
# PI=c(8379.6,8506.6 ,8644.2 ,8707.3 ,8859.0 ,8881.2 ,8880.6 ,8912.3 ,
#      8978.3 ,9060.7 ,9074.6 ,9126.8 ,9194.5 ,9321.1 ,9418.7 ,9578.3 ,
#      9679.8 ,9847.1 ,9999.1 ,10223.1 ,10238.6 ,10386.7 ,10577.5 ,10740.8 ,
#      11026.7 ,11204.0 ,11336.9 ,11504.8 ,11706.9 ,11823.4 ,11945.6 ,
#      12100.3 ,12142.2 ,12292.9 ,12286.6 ,12233.5 )

Quarters = seq(1,3*length(PI),by=3)

plot(Quarters,PI,ylim=c(8200,9000),pch=19,col='red',
     ylab='Personal Income ($Billion)',
     xlab='Months (since Jan 2000)')
axis(1,at=seq(1,3*length(PI)))
grid()
xx = seq(1:(3*length(PI)))

months = seq(Quarters[1],Quarters[length(Quarters)])
months = months[-Quarters]

points(spline(Quarters,PI,xout=months,method='natural'),col='blue',pch=16)

#lines(Quarters,PI,type='l',col='gray')
lines(spline(Quarters,PI,n=500,method='natural'),col='gray')

