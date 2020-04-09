# Looking for the R code in order to analyze a given data set. 
# Need to be created a simple regression model, to evaluate how gender influences the K coefficient. 
# The dependent variable should be K and the independent variables 
# – the rest (gender, age, education, income). 
# I need a fully written R code (including testing multicollinearity, 
#                                testing the normality of distribution, etc.) 
# with short explanations/comments of what is being done, 
# for example: #converting variable gender to factor, #testing multicollinearity, etc. 
# Females are represented by 0 and males by 1. 
# In the end I want to see if women have higher K or lower K than men.

#remove old data
rm(list=ls())

#load libraries
library(readxl)
library(dplyr)

#load excel file
mydata <- read_xlsx(file.choose(),sheet = 1)

#store backup dataframe
mydata.backup <- mydata

summary(mydata)

#drop the first column
mydata[1] <- NULL

#recode Origin
unique(mydata$Origin)
mydata$Origin <- ifelse(mydata$Origin == "Urban", 1, 0)

#recode Education
unique(mydata$Education)
mydata$Education <- ifelse(mydata$Education == "Masters", 1, 0)

#recode employment
unique(mydata$Employment)
mydata <- mydata %>%
  mutate(Employment = as.factor(Employment))
mydata$Employment <- recode_factor(mydata$Employment,"Full time" = '5', 
                                   "Unemployed" = '1', "Student" = '2',  "Master" = '3', 
                                   "Part time" = '4')

#create regression model
#simple linear regression
fit <- lm(mydata$K ~. , data = mydata)
summary(fit)
resid(fit) #List of residuals
plot(density(resid(fit))) #A density plot
qqnorm(resid(fit))

#test for normality
qqPlot(fit, main="QQ Plot")

#check for multicollinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

#anova
model1<- aov(mydata$Gender~mydata$K)
summary(model1)
# we do not reject the null hypothesis based on the p-value and f-statistics

#Now you have to find out the pair of brands which differ. 
#For this you may use the Tukey’s HSD test.
#In R, the following are the code for applying the Tukey’s HSD test:
TukeyHSD(model1, conf.level = 0.99)

plot(TukeyHSD(model1, conf.level = 0.99),las=1, col = "red")