#remove all data
rm(list=ls())

#use filechooser to select csv file
mydata <- read.csv(file.choose(), sep = ',')

#snapshot of data
summary(mydata)

head(mydata)

#hist(mydata$columnname)

#set row names as id labels
#row.names(mydata) <= mydata$keyword

#convert data frame to matrix
myDataMatrix = data.matrix(mydata)

#generate heatmap
heatmap(myDataMatrix, Rowv = NA, Colv = NA,scale = 'column')