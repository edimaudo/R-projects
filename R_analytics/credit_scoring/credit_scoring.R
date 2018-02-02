# remove old data
rm(list=ls())

#load libraries
library(tidyverse)
library(ggplot2)
library(pROC)
library(LogicReg)
library(randomForest)
library(neuralnet)
library(GGally)
library(corrplot)
library(caret)
library(e1071)

#load data
train <- read.csv(file.choose(), sep = ",")
test <- read.csv(file.choose(), sep = ",")

#explore
glimpse(train)
glimpse(test)

names(training)

#remove the first columns
train[1] <- NULL
test[1] <- NULL

#backup 
train.orig <- train
test.orig <- test

#remove na since data imputation would give odd results
train <- na.omit(train)

#correlation
#corinfo <- training[,2:11]
#corrplot(cor(corinfo), method="number")

set.seed(123)

glimpse(train)
glimpse(test)

#modeling

#logistic regression
ctrl <- trainControl(method = "cv", number = 5)
modelglm <- train(as.factor(SeriousDlqin2yrs) ~. , data = train, method = "glm", trControl = ctrl)
summary(modelglm)
### predict
predictors <- names(train)[names(train) != 'SeriousDlqin2yrs']
predglm <- predict(modelglm, train)
summary(predglm)
### score prediction using AUC
confusionMatrix(predglm, train$SeriousDlqin2yrs)

aucglm <- roc(as.numeric(train$SeriousDlqin2yrs), as.numeric(predglm),  ci=TRUE)
plot(aucglm, ylim=c(0,1), print.thres=TRUE, 
     main=paste('Logistic Regression AUC:',round(aucglm$auc[[1]],3)),col = 'blue')

#decision trees
ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(as.factor(SeriousDlqin2yrs) ~., data = train, method = "C5.0Tree", trControl = ctrl)
pred <- predict(tbmodel, train)
confusionMatrix(pred,train$SeriousDlqin2yrs)



