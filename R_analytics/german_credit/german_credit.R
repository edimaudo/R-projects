#libraries
library(ggplot2)
library(corrplot)
library(lattice)
library(dplyr)
library(tidyr)
library(LogicReg)
library(caret)
library(readxl)
library(pROC)
#==========================
#logistic regression
#==========================

#Clear old data
rm(list=ls())

mydata <- read_excel(file.choose())

#summary
summary(mydata)

#Prepare data
mydata.orig = mydata #save orig data copy

mydata <- na.omit(mydata) # listwise deletion of missing

#set seed
set.seed(1)

splitIndex <- createDataPartition(mydata$Default, p = .80,list = FALSE, times = 1)
trainSplit <- mydata[ splitIndex,]
testSplit <- mydata[-splitIndex,]
print(table(trainSplit$Default))


#logistic regression
ctrl <- trainControl(method = "cv", number = 5)
modelglm <- train(Default ~. , data = trainSplit, method = "glm", trControl = ctrl)
summary(modelglm)
### predict
predictors <- names(trainSplit)[names(trainSplit) != 'Default']
predglm <- predict(modelglm, testSplit)
summary(predglm)
### score prediction using AUC
confusionMatrix(predglm, testSplit$Default)

aucglm <- roc(as.numeric(testSplit$Default), as.numeric(predglm),  ci=TRUE)
plot(aucglm, ylim=c(0,1), print.thres=TRUE, 
     main=paste('Logistic Regression AUC:',round(aucglm$auc[[1]],3)),col = 'blue')