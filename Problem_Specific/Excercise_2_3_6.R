A = cbind(c(1,2),c(2,4.01))
b = cbind(c(3,6.01))

#xa = cbind(c(-10,6))
# xa = cbind(c(-100,52))
 xa = cbind(c(-600,301))
#xa = cbind(c(-599,301))

x = cbind(c(1,1))

print("Relative Forward Error:")
fe = x - xa

print(fe)

rfe1 = vnorm(fe, 1)/vnorm(x, 1)
rfe2 = vnorm(fe, 2)/vnorm(x, 2)
rfe3 = vnorm(fe, 'I')/vnorm(x, 'I')

print(rfe1)
print(rfe2)
print(rfe3)

print("Relative Backword Error:")

be = b - (A %*% xa)

print(be)
rbe1 = vnorm(be, 1)/vnorm(b, 1)
rbe2 = vnorm(be, 2)/vnorm(b, 2)
rbe3 = vnorm(be, 'I')/vnorm(b, 'I')

print(rbe1)
print(rbe2)
print(rbe3)

print("Magnification:")
print (rfe1/rbe1)
print (rfe2/rbe2)
print (rfe3/rbe3)



