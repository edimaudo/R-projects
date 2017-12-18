#load libraries
library(cluster)
library(ggplot2)
library(factoextra)
library(corrplot)
library(psy)
library(lattice)
library(nFactors)
library(RColorBrewer)
library(scales)


#remove all data
rm(list=ls())

#use filechooser to select csv file
mydata <- read.csv(file.choose(), sep = ',')

#simple linear regression
output <- lm(LSI ~ CORRUPT, data = mydata)
summary(output)
resid(output) #List of residuals
plot(density(resid(output))) #A density plot
qqnorm(resid(output))

output2 <- lm(log(LSI) ~ CORRUPT, data = mydata)
summary(output2)
resid(output2) #List of residuals
plot(density(resid(output2))) #A density plot
qqnorm(resid(output2))


