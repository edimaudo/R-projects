

#remove all data
rm(list=ls())

#use filechooser to select csv file
mydata <- read.csv(file.choose(), sep = ',')

#view data
view(mydata)

#snapshot of data
summary(mydata)


#plot(mydata$broadcast,mydata$netsales)

#fit a line
#myLM = lm(mydata$netsales ~ mydata$broadcast)

#visualize the line
#lines(mydata$broadcast, myLM$fitted)

#show the coefficients
#myLM$coeff