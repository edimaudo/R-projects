#libraries

#remove all data
rm(list=ls())

#use filechooser to select csv file
mydata <- read.csv(file.choose(), sep = ',')

#view data
head(mydata)

#general summary
summary(mydata)



