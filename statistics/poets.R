#remove all data
rm(list=ls())

#use filechooser to select csv file
mydata <- read.csv(file.choose(), sep = ',')

str(mydata)

summary(mydata)

#simple linear regression
output <- lm(mydata$agedeath ~ mydata$writer, data = mydata)
summary(output)
resid(output) #List of residuals
plot(density(resid(output))) #A density plot
qqnorm(resid(output))

#anova
model1<- aov(mydata$writer~mydata$agedeath)
summary(model1)
# we do not reject the null hypothesis based on the p-value and f-statistics

#Now you have to find out the pair of brands which differ. 
#For this you may use the Tukey’s HSD test.
#In R, the following are the code for applying the Tukey’s HSD test:
TukeyHSD(model1, conf.level = 0.99)

plot(TukeyHSD(model1, conf.level = 0.99),las=1, col = "red")

par(mfrow=c(2,2))
plot(model1)

#normality chck
uhat<-resid(model1)
shapiro.test(uhat)