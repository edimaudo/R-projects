#remove all data
rm(list=ls())

#use filechooser to select csv file
mydata <- read.csv(file.choose(), sep = ',')

#simple linear regression
output <- lm(Gesell.score ~ Age, data = mydata)
summary(output)
resid(output) #List of residuals
plot(density(resid(output))) #A density plot
qqnorm(resid(output))