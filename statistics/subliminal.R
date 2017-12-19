#remove all data
rm(list=ls())

#use filechooser to select csv file
mydata <- read.csv(file.choose(), sep = ',')

str(mydata)

summary(mydata)

library(ggplot2)
ggplot(mydata, aes(pre, post)) + geom_point(aes(colour = factor(group)))
ggplot(mydata, aes(group, pre)) + geom_boxplot()

output <- wilcox.test(mydata$pre, mydata$post)
res1 <- wilcox.test(post ~ group, data = my_data,
                   exact = FALSE)
res2 <- wilcox.test(post ~ group, data = mydata,
                   exact = FALSE)