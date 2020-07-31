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
#United Aluminum Company of Cincinnati produces three grades (high, medium, and low) of aluminum at two mills. 
#Each mill has a different production capacity (in tons per day) for each grade


