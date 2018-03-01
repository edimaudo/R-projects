#remove all data
rm(list=ls())

#load libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)
library(colorspace)


#list of files
file_names = c(
  "SceneAnalytics.dbo.LK_account_unique_member_identifier_sample10.csv",

)

#update working directory
setwd("/Users/edima/Documents/Queens/courses/big_data_865/project/sample02/") 

#clean filenames
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
#load the files
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

#clean data

#build model



#-----------------
#models
#-----------------
library(mlbench)
library(caret)
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(123)
#logistic regression
fit.glm <- train(status~., data=kiva_no_text, method="glm", trControl=control)
#decision trees
fit.cart <- train(status~., data=kiva_no_text, method="rpart", trControl=control)
#LDA
fit.lda <- train(status~., data=kiva_no_text, method="lda", trControl=control)
#svm
fit.svm <- train(status~., data=kiva_no_text, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(status~., data=kiva_no_text, method="rf", trControl=control)
#bagged cart
fit.treebag <- train(status~., data=kiva_no_text, method="treebag", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(status~., data=kiva_no_text, method="gbm", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(logistic = fit.glm, cart = fit.cart, lda = fit.lda, 
                          svm = fit.svm, randomforest = fit.rf, 
                          baggedcart = fit.treebag, gradboost = fit.gbm))
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

# density plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")

# pair-wise scatterplots of predictions to compare models
splom(results)

#statistical significance for algorithms
# difference in model predictions
diffs <- diff(results)
# summarize p-values for pair-wise comparisons
summary(diffs)
