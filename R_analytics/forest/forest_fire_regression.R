#remove all data
rm(list=ls())

#libraries
library(readxl)
library(tidyverse)
library(caret)
library(mlbench)
library(data.table)

#load data
df = read.csv(file.choose())

#view data
glimpse(df)

#check for missing data
apply(df, 2, function(x) any(is.na(x))) #no missing data

#set seed for reproducability
set.seed(123)

#recode day and month
levels(df$day)
levels(df$month)

df1 <- df

#recode day
df1$day = recode_factor(df1$day, 'mon'='1','tue'='2','wed'='3','thu'='4','fri'='5',
                       'sat'='6','sun'='7')
df1$day = as.numeric(as.character(df1$day))

#recode month
df1$month = recode_factor(df1$month, 'jan'='1','feb'='2','mar'='3','apr'='4','may'='5',
                         'jun'='6','jul'='7','aug'='8','sep'='9','oct'='10','nov'='11','dec'='12')
df1$month = as.numeric(as.character(df1$month))

#check for correlations
library(corrplot)
corrplot(cor(df1), method = "number")

#split data into training and test
train<-sample_frac(df, 0.8)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-df[-sid,]

#build initial model and test
linearMod <- lm(area ~., data=train)
summary(linearMod)
AIC(linearMod)

testPred <- predict(linearMod, test)
actuals_preds <- data.frame(cbind(actuals=test$area, predicteds=testPred))
correlation_accuracy <- cor(actuals_preds) 
%>% 