##This file contain solution for Exam 2 Question 5

######################### ranking function ###################
# Takes an matrix A and vector b
# Print out xbar as the solution and the ranking
scoreRank = function(A,b){
  # We need to use least square fit, because there is more equation than unknown
  xbar = solve(t(A)%*%A, t(A)%*%b) # this x bar, normal equation
  print("ranking scores:")
  print(t(xbar))
  print("ranking")
  print(order(xbar,decreasing=TRUE))
  return(xbar)
}

######################### Problem (a) ########################

# A is a matrix that captures result of the games
A = cbind(c(1,0,-1,0,1,1),c(-1,0,0,1,0,1),c(0,1,1,0,0,1),c(0,-1,0,-1,-1,1))
b = c(4,3,9,7,6,100) #b is the result
print("Ranking Problem (a)")
scoreRank(A=A,b=b)

######################### Problem (b) ###########################

# This year's recreational intramural soccer

A = rbind(c(1,0,0,0,0,0,0,-1),
          c(0,0,0,1,-1,0,0,0),
          c(0,1,0,0,0,0,-1,0),
          c(0,0,1,0,0,-1,0,0),
          c(-1,0,0,1,0,0,0,0),
          c(0,1,-1,0,0,0,0,0),
          c(0,1,0,-1,0,0,0,0),
          c(1,1,1,1,1,1,1,1))
b = c(2,4,4,2,3,4,1,100)
print("Ranking Problem (b)")
scoreRank(A=A,b=b)








