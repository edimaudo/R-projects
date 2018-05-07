# loading package
library(qcc)
# Loading the piston rings data
data(pistonrings)
attach(pistonrings)
head(pistonrings)

diameter <- qcc.groups(diameter, sample)
head(diameter)
summary(diameter)

obj <- qcc(diameter[1:30,], type="R")
summary(obj)
obj <- qcc(diameter[1:30,], type="xbar")
summary(obj)
obj <- qcc(diameter[1:30,], type="xbar",nsigmas = 2)
summary(obj)
obj <- qcc(diameter[1:30,], type="xbar",nsigmas = 1)

#test new data
obj <- qcc(diameter[1:30,], type="xbar",newdata=diameter[31:40,])