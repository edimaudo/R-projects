#remove all data
rm(list=ls())

#use filechooser to select csv file
mydata <- read.csv(file.choose(), sep = ',')

str(mydata)

summary(mydata)

output <- kruskal.test(strength ~ weeks, data = mydata)
print(output)