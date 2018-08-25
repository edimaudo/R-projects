#Clear old data
rm(list=ls())

#libraries
library(ggplot2)
library(corrplot)
library(lattice)
library(tidyverse)
library(caret)
library(mlbench)
library(readxl)

mydata <- read_excel(file.choose())

#summary
summary(mydata)

#Prepare data
mydata.orig <- mydata #save orig data copy

mydata <- na.omit(mydata) # listwise deletion of missing



#check for balanced data
ggplot(data=mydata, aes(factor(Default))) + geom_bar() + theme_classic()
#lot more 0s than 1

#data balance
#for unbalanced data set#
library(unbalanced)
n<-ncol(mydata)
output<- mydata$D
output<-as.factor(output)
input<- rareevent_boost [ ,-n]
View(input)

#Balance the Dataset using ubSMOTE#
data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE
                View(data)
                
                #Balanced Data#
                balancedData<-cbind(data$X,data$Y)
                View(balancedData)
                table(balancedData$CHURN_FLAG)

#split data into categorical and non categorical data

my_data_category <- mydata %>%
  select()

mydata_predict <- mydata %>%
  select()

mydata_not_category <- mydata %>%
  select()

#normalization function

#normalize cts variables

#remove unnecessary features
#set seed
set.seed(1)

#combine category, non category and predict

#split data in test and train
#set seed
set.seed(1)