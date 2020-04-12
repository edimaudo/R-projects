#LP MODEL
#8A + 7B
#3A + 8B >= 6
#6A + 4B >= 4.5
#4A + 6B >= 5

library(lpSolve)

#decision variables
C <- c(8,7)

#constraint matrix
A <- matrix(c(3,6,4,8,4,6),nrow = 3,byrow = TRUE)

B <- c(6,4.5,5)

constraint_direction <- c(">=",">=",">=")

# Find the optimal solution
optimum <-  lp(direction="min",
               objective.in = C,
               const.mat = A,
               const.dir = constraint_direction ,
               const.rhs = B,
               all.int = T)

# Print status: 0 = success, 2 = no feasible solution
print(optimum$status)

# Check the value of objective function at optimal point
print(paste("Total cost: ", optimum$objval, sep=""))