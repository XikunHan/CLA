# HW6. Problem 1. Modeled after Computer Problem 3.4.14 in the book
# closing price of Yahoo stock taken from 
# http://finance.yahoo.com/q/hp?s=YHOO
# for the 101 trading days ending on 3/27/2013

################### Setting up the Problem ####################
yahoo=c(22.4,  22.6,  22.9,  22.7,  22.8,  
        22.95,  22.7,  21.94,  21.31,  21.16,  20.76,  20.73,  21.22,  
        20.83,  20.92,  21.29,  21.02,  21.18,  21.15,  21.21,  20.9,  
        20.5,  20.32,  19.85,  19.66,  19.34,  19.76,  19.63,  20.12,  
        19.7,  20.31,  20.37,  20.44,  20.11,  19.9,  20.02,  20.13,  
        20.07,  19.52,  19.43,  19.29,  18.99,  19.32,  19.66,  19.4,  
        19.86,  19.78,  20.08,  19.9,  19.5,  19.6,  19.57,  19.65,  
        19.35,  19.69,  19.6,  19.62,  19.69,  19.64,  19.35,  19.38,  
        19.52,  19.43,  19.2,  19.2,  18.89,  18.93,  18.55,  18.77,  
        18.87,  18.91,  18.93,  18.76,  18.57,  18.4,  18.24,  18.36,  
        17.86,  17.89,  17.83,  17.85,  17.51,  17.26,  17.24,  17.39,  
        17.46,  17.37,  17.11,  16.95,  16.84,  16.79,  16.61,  16.55,  
        16.67,  15.77,  15.84,  16,  16.09,  15.92,  15.68,  15.88)
days = 0:100

# sample every 5th day
days5 = seq(0,100,by=5)
yahoo5 = yahoo[days5+1]

plot(days,yahoo,pch=20,cex=.25,ylim=c(14,25),
     ylab = "YHOO Closing Price",
     xlab = "Consecutive trading days up to 3/27/13",
     main = "Yahoo Stock Prices 2013")
grid()
points(days5,yahoo5,pch=20,cex=.5,col='red')

################## Interpolating ##############################
ix = seq(0,100) # x sequence for ploting interpolating polynomial 
ivalues = Interpolator(xx=days5, yy=yahoo5, ix) # y sequence for ploting
lines(ix,ivalues, col='seagreen4')

### Finding Error
ierr = ivalues-yahoo
print("Infinity norm of Interpolating Polynomial is ")
print(vnorm(ierr, p='I'))
print("1-norm of Interpolating Polynomial is ")
print(vnorm(ierr, p=1))

################## Cubic Spline ###############################
result = spline(days5,yahoo5,n=101,method='natural')
sx = result$x;
svalues = result$y
lines(result,col='blue')


### Finding Error
serr = svalues-yahoo
print("Infinity norm of Cubic Spline is ")
print(vnorm(serr, p='I'))
print("1-norm of Cubic Spline is ")
print(vnorm(serr, p=1))







