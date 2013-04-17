x = c(-2,-1.5,-.9,0.3,1,2.1,3.7,4,5)
y = c(4,4,3,5,0,-2,2,0,0)

plot(x,y,pch=19,col='dodgerblue',cex=1.2,ylim=c(min(y)-1,max(y)+1))
grid()

############### Making Spine on Data Points x and y ###############
lines(spline(x,y,n=300,method='natural'),col='seagreen4')
# lines(spline(x,y,n=300,method='fmm'),col='red')

#explore using xout, too.
# use xout for you own points or n=300 for 300 evenly spaced
points(spline(x,y,xout=c(1.1,1.2,1.3),method='fmm'),col='black')

# n = 10
# x = sort(runif(n))
# y = cumsum(abs(rnorm(n)))
# plot(x,y,pch=20)
# lines(spline(x,y,n=100,method="natural"),col=2)
# lines(spline(x,y,n=100,method="hyman"),col=3)