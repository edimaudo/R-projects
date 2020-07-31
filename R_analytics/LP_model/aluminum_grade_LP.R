#clear environment
rm(list = ls())

#packages 
packages <- c('lpSolve')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#objective 
#Find out the number of days to operate 
#each mill to meet the contract at the minimum cost
#United Aluminum Company of Cincinnati produces three grades (high, medium, and low) of 
#aluminum at two mills. 
#Each mill has a different production capacity (in tons per day) for each grade

objective.in=c(6000,7000) #costs for mills

#Constraint Matrix
const.mat=matrix(c(6,2,2,2,4,10),nrow = 3,byrow = T) #high, medium, low grade

#defining constraints
const_high=12  
const_medium=8
const_low =5

#RHS for constraints
const.rhs=c(const_high, const_medium, const_low)

#Direction for constraints
const.dir=c(">=",">=",">=")


#Finding the optimum solution
opt=lp(direction = "min",objective.in,const.mat,const.dir,const.rhs)
summary(opt)


#Objective values
opt$solution

#Value of objective function at optimal point
opt$objval

