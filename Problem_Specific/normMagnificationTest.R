p = 2
A = cbind(c(1,1.0001),c(1,1))
b = c(2, 2.0001)
xa = c(-1,3.0001) #computed approx answer
x = c(1,1) #correct answer

relFE = vnorm(x-xa, p)/vnorm(x,p) #relative forward error
print("Forward Error")
print(x-xa)
print("Relative Foward Error")
print(relFE)

relBE = vnorm((b - A%*%xa),p)/vnorm(b,p) #relative backward error
print("Backwrad Error")
print(b - A%*%xa)
print("Relative Backward Errord")
print(relBE)

mag = relFE/relBE
print("Magnification")
print(mag)