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
library(kernlab)

#load data
train <- read.csv(file.choose(), sep = ",")
test <- read.csv(file.choose(), sep = ",")

#explore
glimpse(train)
glimpse(test)

names(training)

#backup 
train.orig <- train
test.orig <- test

#remove the first columns
train[1] <- NULL
test[1] <- NULL

#remove na
train <- na.omit(train)
test <- na.omit(test)

set.seed(123)

glimpse(train)
glimpse(test)

#modeling

# #logistic regression
ctrl <- trainControl(method = "cv", number = 5)
modelglm <- train(as.factor(SeriousDlqin2yrs) ~. , data = train, method = "glm", trControl = ctrl)
summary(modelglm)
### predict
predictors <- names(train)[names(train) != 'SeriousDlqin2yrs']
predglm <- predict(modelglm, train)
summary(predglm)
### score prediction using AUC
confusionMatrix(predglm, train$SeriousDlqin2yrs)

# aucglm <- roc(as.numeric(train$SeriousDlqin2yrs), as.numeric(predglm),  ci=TRUE)
# plot(aucglm, ylim=c(0,1), print.thres=TRUE, 
#      main=paste('Logistic Regression AUC:',round(aucglm$auc[[1]],3)),col = 'blue')

# #decision trees
ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(as.factor(SeriousDlqin2yrs) ~., data = train, method = "C5.0Tree", trControl = ctrl)
pred <- predict(tbmodel, train)
confusionMatrix(pred,train$SeriousDlqin2yrs)
# 
# # Linear Discriminant Analysis with Jacknifed Prediction
library(MASS)
fit <- lda(train$SeriousDlqin2yrs ~., data=train,
           na.action="na.omit", CV=TRUE)
# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(train$SeriousDlqin2yrs, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

library(party)
# create model using random forest and bagging ensemble using conditional inference trees
x.cf <- cforest(train$SeriousDlqin2yrs ~ ., data=train, control = cforest_unbiased(mtry = ncol(train)-2))
x.cf.pred <- predict(x.cf, newdata=BreastCancer[ind == 2,])
x.cf.prob <-  1- unlist(treeresponse(x.cf, BreastCancer[ind == 2,]), use.names=F)[seq(1,nrow(BreastCancer[ind == 2,])*2,2)]

# create model using bagging (bootstrap aggregating)
require(ipred)
x.ip <- bagging(Class ~ ., data=BreastCancer[ind == 1,])
x.ip.prob <- predict(x.ip, type="prob", newdata=BreastCancer[ind == 2,])

# # create model using recursive partitioning on the training data set
# require(rpart)
# x.rp <- rpart(Class ~ ., data=BreastCancer[ind == 1,])
# # predict classes for the evaluation data set
# x.rp.pred <- predict(x.rp, type="class", newdata=BreastCancer[ind == 2,])
# # score the evaluation data set (extract the probabilities)
# x.rp.prob <- predict(x.rp, type="prob", newdata=BreastCancer[ind == 2,])
# 
# # To view the decision tree, uncomment this line.
# # plot(x.rp, main="Decision tree created using rpart")
# 
# # create model using conditional inference trees
# require(party)
# x.ct <- ctree(Class ~ ., data=BreastCancer[ind == 1,])
# x.ct.pred <- predict(x.ct, newdata=BreastCancer[ind == 2,])
# x.ct.prob <-  1- unlist(treeresponse(x.ct, BreastCancer[ind == 2,]), use.names=F)[seq(1,nrow(BreastCancer[ind == 2,])*2,2)]
# 
# # To view the decision tree, uncomment this line.
# # plot(x.ct, main="Decision tree created using condition inference trees")
# 
# # create model using random forest and bagging ensemble using conditional inference trees
# x.cf <- cforest(Class ~ ., data=BreastCancer[ind == 1,], control = cforest_unbiased(mtry = ncol(BreastCancer)-2))
# x.cf.pred <- predict(x.cf, newdata=BreastCancer[ind == 2,])
# x.cf.prob <-  1- unlist(treeresponse(x.cf, BreastCancer[ind == 2,]), use.names=F)[seq(1,nrow(BreastCancer[ind == 2,])*2,2)]
# 
# # create model using bagging (bootstrap aggregating)
# require(ipred)
# x.ip <- bagging(Class ~ ., data=BreastCancer[ind == 1,])
# x.ip.prob <- predict(x.ip, type="prob", newdata=BreastCancer[ind == 2,])
# 
# # create model using svm (support vector machine)
# require(e1071)
# # svm requires tuning
# x.svm.tune <- tune(svm, Class~., data = BreastCancer[ind == 1,],
#                    ranges = list(gamma = 2^(-8:1), cost = 2^(0:4)),
#                    tunecontrol = tune.control(sampling = "fix"))
# # display the tuning results (in text format)
# x.svm.tune
# # If the tuning results are on the margin of the parameters (e.g., gamma = 2^-8), 
# # then widen the parameters.
# # I manually copied the cost and gamma from console messages above to parameters below.
# x.svm <- svm(Class~., data = BreastCancer[ind == 1,], cost=4, gamma=0.0625, probability = TRUE)
# x.svm.prob <- predict(x.svm, type="prob", newdata=BreastCancer[ind == 2,], probability = TRUE)
# 
# 
# 
# #model comparison
# # Output the plot to a PNG file for display on web.  To draw to the screen, 
# # comment this line out.
# png(filename="roc_curve_5_models.png", width=700, height=700)
# 
# # load the ROCR package which draws the ROC curves
# require(ROCR)
# 
# # create an ROCR prediction object from rpart() probabilities
# x.rp.prob.rocr <- prediction(x.rp.prob[,2], BreastCancer[ind == 2,'Class'])
# # prepare an ROCR performance object for ROC curve (tpr=true positive rate, fpr=false positive rate)
# x.rp.perf <- performance(x.rp.prob.rocr, "tpr","fpr")
# # plot it
# plot(x.rp.perf, col=2, main="ROC curves comparing classification performance of five machine learning models")
# 
# # Draw a legend.
# legend(0.6, 0.6, c('rpart', 'ctree', 'cforest','bagging','svm'), 2:6)
# 
# # ctree
# x.ct.prob.rocr <- prediction(x.ct.prob, BreastCancer[ind == 2,'Class'])
# x.ct.perf <- performance(x.ct.prob.rocr, "tpr","fpr")
# # add=TRUE draws on the existing chart 
# plot(x.ct.perf, col=3, add=TRUE)
# 
# # cforest
# x.cf.prob.rocr <- prediction(x.cf.prob, BreastCancer[ind == 2,'Class'])
# x.cf.perf <- performance(x.cf.prob.rocr, "tpr","fpr")
# plot(x.cf.perf, col=4, add=TRUE)
# 
# # bagging
# x.ip.prob.rocr <- prediction(x.ip.prob[,2], BreastCancer[ind == 2,'Class'])
# x.ip.perf <- performance(x.ip.prob.rocr, "tpr","fpr")
# plot(x.ip.perf, col=5, add=TRUE)
# 
# # svm
# x.svm.prob.rocr <- prediction(attr(x.svm.prob, "probabilities")[,2], BreastCancer[ind == 2,'Class'])
# x.svm.perf <- performance(x.svm.prob.rocr, "tpr","fpr")
# plot(x.svm.perf, col=6, add=TRUE)







