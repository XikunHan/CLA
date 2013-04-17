#Computer Problem 3.2: 3

############## Setting up the Problem ##############
year = seq(1994,2003)
bbld = c(67.052,68.008,69.803,72.024,73.400,72.063,74.669,74.487,74.065,76.77)

############## Running Interpolation ###########
v = Interpolator(year,bbld,2010,method="NewtonDD") #Interpolation value at year=2010
print("Value at 2010:")
print(v) #-1951726

# get the interpolation function p
c = NewtonDD(year,bbld) 
p = function(z) { 
  Horner(c,z,year) 
}

############# Plotting Runge phenomenon ############

#Plotting Interpolation Function
#range from year = 1990 to 2010
#Runge phenomenon
xx = seq(1990,2010,length=100) #change this to zoom out
plot(xx,p(xx),type='l',ylab="bbl/day(in millions)")

#Ploting original points
points(year,bbld,col='red',pch=20,cex=1.5,ylab="bbl/day(in millions)")

############# Plotting Narrower Range ############

#Plotting Interpolation Function
#range from year = 1993 to 2003
xx = seq(1994,2003,length=100) #change this to zoom out
plot(xx,p(xx),type='l',ylab="bbl/day(in millions)")

#Ploting original points
points(year,bbld,col='red',pch=20,cex=1.5)




