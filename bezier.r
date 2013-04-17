# This function returns points along a Bezier spline with given
# endpoints (x1,y1) and (x4,y4) and control points (x1,y2) and (x3,y3)
one.bezier = function(x1, x2, x3, x4, y1, y2, y3, y4, npts = 10) {
  bx = 3 * (x2 - x1)
  cx = 3 * (x3 - x2) - bx
  dx = x4 - x1 - bx - cx
  by = 3 * (y2 - y1)
  cy = 3 * (y3 - y2) - by
  dy = y4 - y1 - by - cy
  t = seq(0, 1, length = npts)
  x = x1 + t * (bx + t * (cx + dx * t))
  y = y1 + t * (by + t * (cy + dy * t))
  return(list(x = x, y = y))
}

# This function draws a single Bezier curve with given
# endpoints (x1,y1) and (x4,y4) and control points (x1,y2) and (x3,y3)
draw.one.bezier = function(x1, x2, x3, x4, y1, y2, y3, y4, npts = 10,new = TRUE, ...) {
  pts = one.bezier(x1, x2, x3, x4, y1, y2, y3, y4, npts = npts)
  if (new) 
    plot(pts, type = "n", 
         xlim = c(min(pts$x, x1, x2, x3, x4),max(pts$x, x1, x2, x3, x4)), 
         ylim = c(min(pts$y, y1, y2, y3, y4),max(pts$y, y1, y2, y3, y4)),...)
  lines(pts, ...)
  points(c(x1, x4), c(y1, y4), pch = 20, ...)
  points(c(x2, x3), c(y2, y3), pch = 10, ...)
  lines(c(x1, x2), c(y1, y2), lty = 3)
  lines(c(x4, x3), c(y4, y3), lty = 3)
  invisible(pts)
}

# This function draws multiple Bezier curves on the same plot given vectors of x and y points
draw.beziers = function(x1, x2, x3, x4, y1, y2, y3, y4, npts = 10,new = TRUE, ...) {
  allx = c(x1, x2, x3, x4)
  ally = c(y1, y2, y3, y4)
  xlim = c(min(allx), max(allx))
  ylim = c(min(ally), max(ally))
  if (new) 
    plot(1:2, xlim = xlim, ylim = ylim,...)
  xpts = c()
  ypts = c()
  for (k in 1:length(x1)) {
    pts = draw.one.bezier(x1[k], x2[k], x3[k], x4[k], y1[k], 
                          y2[k], y3[k], y4[k], npts=npts,new = FALSE, xlim = xlim, ylim = ylim, 
                          ...)
    xpts = c(xpts, pts$x)
    ypts = c(ypts, pts$y)
  }
  invisible(list(x = xpts, y = ypts))
}

# This function allows the user to manipulate a Bezier curve.  You must include the manipulate package
bezier.interact = function(x1=0,y1=0,x2=10,y2=10){
  ang1 = 0;
  ang2 = 0;
  len1 = 1;
  len2 = 1;
  do.it = function(len1,len2,ang1,ang2) {
    cx1 = x1 + len1*cos(ang1)
    cy1 = y1 + len1*sin(ang1)
    cx2 = x2 + len2*cos(ang2)
    cy2 = y2 + len2*sin(ang2)
    #    foo = make.all.bezier(cbind(x1,y1,cx1,cy1,cx2,cy2,x2,y2))
    draw.one.bezier(x1,cx1,cx2,x2,y1,cy1,cy2,y2,npts=100,xlab="",ylab="")
  }
  manipulate( do.it(len1,len2,ang1*pi/180, ang2*pi/180), 
              len1=slider(0,10,step=.1,initial=1),
              ang1 = slider(-180,180,step=1,initial=-90),
              len2 = slider(0,10,step=.1, initial=1),
              ang2 = slider(-180,180,step=1,initial=90) )
}

# This function takes a matrix T of points in 
# x1, y1, x2, y2, x3, y3, x4, y4 form and draws the Bezier curve
# See the example below
draw.curve = function(T, npts = 100, ...) {
  draw.beziers(T[, 1], T[, 3], T[, 5], T[, 7], T[, 2],T[, 4], T[, 6], T[, 8],npts=npts,xlab="",ylab="")
}

#  --------------------- Example ----------------------------
T = rbind(
  c(150,640,150,600,200,520,250,600),
  c(250,600,285,660,300,620,350,620),
  c(350,620,400,620,400,700,350,680),
  c(350,680,300,660,150,700,150,640))

T2 = rbind( #adding a new point
  c(150,640,150,600,200,520,250,600),
  c(250,600,285,660,300,620,350,620),
  c(350,620,400,620,400,700,350,680),
  c(350,680,400,620,400,700,200,670),#new point added here
  c(200,670,300,660,150,700,150,640))

#draw.curve(T)
draw.curve(T2)
