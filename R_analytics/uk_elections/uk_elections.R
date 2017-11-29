#libraries
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
library(NbClust)
library(ggpubr)

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
