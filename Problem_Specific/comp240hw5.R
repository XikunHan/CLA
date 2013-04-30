# This R program solves Comp 240 Homework 5 
xx = c(1,2,4,8,16,32)
yy = c(0.715983, 0.358588, 0.200269, 0.115521, 0.0932389, 0.0943575)
yy = 0.715983/yy
x = seq(1,32,length=100)
plot(spline(xx,yy,n=300,method='natural'),col='seagreen4',type='l')
points(xx,yy)
#plot(xx,yy)