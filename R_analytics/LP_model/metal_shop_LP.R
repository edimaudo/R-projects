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

#objective mininmize cost
#Setting the coefficients of decision variables
objective.in=c(500,400) #cost

#Constraint Matrix
const.mat=matrix(c(3,4,7,2,3,2),nrow = 3,byrow = T)

#defining constraints
const_iron=80  
const_glass=60
const_labor =50

#RHS for constraints
const.rhs=c(const_iron, const_glass, const_labor)

#Direction for constraints
const.dir=c(">=",">=",">=")


#Finding the optimum solution
opt=lp(direction = "min",objective.in,const.mat,const.dir,const.rhs)
summary(opt)


#Objective values
opt$solution

#Value of objective function at optimal point
opt$objval
