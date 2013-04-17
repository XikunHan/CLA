# For this to work you need to source the file bezier.r 

letter.T = rbind(
  c(237, 620, 237, 620, 237, 120, 237, 120), 
  c(237,120, 237, 35, 226, 24, 143, 19), 
  c(143, 19, 143, 19, 143, 0, 143, 0),
  c(143, 0, 143, 0, 435, 0, 435, 0), 
  c(435, 0,435, 0, 435, 19, 435, 19),A =  
    c(435, 19, 353, 23, 339, 36, 339,109), 
  c(339, 109, 339, 108, 339, 620, 339, 620), 
  c(339, 620,339, 620, 393, 620, 393, 620), 
  c(393, 620, 507, 620, 529,602, 552, 492), 
  c(552, 492, 552, 492, 576, 492, 576, 492), 
  c(576, 492, 576, 492, 570, 662, 570, 662), 
  c(570, 662, 570,662, 6, 662, 6, 662), 
  c(6, 662, 6, 662, 0, 492, 0, 492),
  c(0, 492, 0, 492, 24, 492, 24, 492), 
  c(24, 492, 48, 602,71, 620, 183, 620), 
  c(183, 620, 183, 620, 237, 620, 237,620))

number.five = rbind(
  c(149,597,149,597,149,597,345,597),
  c(345,597,361,597,365,599,368,606),
  c(368,606,406,695,368,606,406,695),
  c(406,695,397,702,406,695,397,702),
  c(397,702,382,681,372,676,351,676),
  c(351,676,351,676,351,676,142,676),
  c(142,676,33,439,142,676,33,439),
  c(33,439,32,438,32,436,32,434),
  c(32,434,32,428,35,426,44,426),
  c(44,426,74,426,109,420,149,408),
  c(149,408,269,372,324,310,324,208),
  c(324,208,324,112,264,37,185,37),
  c(185,37,165,37,149,44,119,66),
  c(119,66,86,90,65,99,42,99),
  c(42,99,14,99,0,87,0,62),
  c(0,62,0,24,46,0,121,0),
  c(121,0,205,0,282,27,333,78),
  c(333,78,378,123,399,180,399,256),
  c(399,256,399,327,381,372,333,422),
  c(333,422,288,468,232,491,112,512),
  c(112,512,112,512,149,597,149,597))

draw.letter = function(T, npts = 10, ...) {
  pts = draw.beziers(T[, 1], T[, 3], T[, 5], T[, 7], T[, 2],T[, 4], T[, 6], T[, 8], npts = npts, ...)
  polygon(pts, col = rgb(1, 0, 0, 0.3),border="purple",xlab=" ",ylab=" ") #rgb shades the inside
}

draw.letter(letter.T)
draw.letter(number.five)