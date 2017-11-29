#libraries
library(corrplot)
library(cluster)
library(ggplot2)
library(factoextra)
library(lattice)
library(dplyr)    


#Q1
scores <- c(98, 74, 65, 78, 17, 65, 82, 72, 68, 
            74, 59, 95, 51, 94, 39, 34, 68, 17, 90, 
            86, 65, 57, 33, 78, 29, 80, 65, 29, 82, 
            46, 55, 50, 32, 43, 90, 3)

mean_scores <- mean(scores)
std_scores <- sd(scores)

score_amount <- length(scores)

#95% confidence interval
error <- qnorm(0.975)*std_scores/sqrt(score_amount)
left95 <- mean_scores - error
right95 <- mean_scores + error
n95 <- c(left95,right95)

#99% confidence interval
error99 <- qnorm(0.995)*std_scores/sqrt(score_amount)
left99 <- mean_scores - error99
right99 <- mean_scores + error99
n99 <- c(left99,right99)

#Q2 
#use unpaired two sample t-test
#The unpaired two-samples t-test is used to compare the mean of two independent groups.

#hypothesis test
#whether the mean of group A (mAmA) is greather than the mean of group B (mBmB)?
#can't compute do welch t-test manually

#Q4
#look at the p-values of the different coefficients to explain the impact
#Also look at the r-square value to see how much it interrpets

#Q3
mydata <- results
View(mydata)

#summary for LD17
summary_LD17 <- summary(mydata$LD17)
print(summary_LD17)

#summary for Winner2015
summary_Winner2015 <- summary(mydata$Winner2015)
print(summary_Winner2015)

#scatterplot of Conservative vote share in 2017 in each constituency and PctHomeowners
ggplot(mydata, aes(x=Con17, y=PctHomeOwners)) + geom_point()

#scatterplot of Conservative vote share in 2017 in percentage of 
#unemployed people in each constituency (PctUnemployed)
ggplot(mydata, aes(x=Con17, y=PctUnemployed)) + geom_point() + theme_minimal()

#boxplot of which depicts the distribution of the Labour Party’s constituency-level 
#vote share in each UK region (Region)
ggplot(mydata, aes(Region, Lab17)) + geom_boxplot() + theme_minimal()

#mean difference in the Labour Party’s share of the vote (Lab17) in 
#England and in Scotland
countryEnglanddata <- filter(mydata, Country == "E")
countryScotlanddata <- filter(mydata, Country == "S")

countryEnglandLab17 <- countryEnglanddata$Lab17
countryScotlandLab17 <- countryScotlanddata$Lab17

mean_difference <- t.test(countryEnglandLab17, countryScotlandLab17, mu = 0, conf = .95)
print(mean_difference)

# linear regression 
#dependent variable is the Liberal Democrat share of 
#the vote in a constituency (LD17 ), and the independent variable 
#is the percentage of the population of a constituency 
#with high levels of education (PctHighEducation).

fit1 <- lm(LD17 ~ PctHighEducation, data = mydata)
summary(fit1)

ggplot(mydata, aes(x = PctHighEducation, y = LD17)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")