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
#Q1

#
scores <- c(98, 74, 65, 78, 17, 65, 82, 72, 68, 
            74, 59, 95, 51, 94, 39, 34, 68, 17, 90, 
            86, 65, 57, 33, 78, 29, 80, 65, 29, 82, 
            46, 55, 50, 32, 43, 90, 3)

mean_scores <- mean(scores)
std_scores <- sd(scores)

score_amount <- length(scores)

error <- qnorm(0.975)*std_scores/sqrt(score_amount)
left <- mean_scores - error
right <- mean_scores + error

#test for 95% and 99% confidence interval

#Q2 compare the difference in means
#what is the hypothesis ->
# might wantnt o use t -test
