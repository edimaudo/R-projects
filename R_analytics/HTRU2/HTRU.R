#exploratory analysis
#feature engineering
#use cross validation



#HTRU dataset - http://archive.ics.uci.edu/ml/datasets/HTRU2

#objective

#clear old data
rm(list=ls())

#load libraries
library(tidyverse)
library(caret)
library(mlbench)
library(ggplot2)
library(corrplot)

#load HTRU data
mydata <- read.csv(file.choose())

mydata <- as.data.frame(mydata)

#name columns
names(mydata) <- c("mean_ip", "standard_deviation_ip", "excess_kurtosis_ip",
                   "skewness_ip","mean_DMSNR","standard_deviation_DMSNR",
                 "excess_kurtoisis_DMSNR","skewness_DMSNR","class")

#backup data
mydata.orig <- mydata

#summary
summary(mydata)

#correlation plot
corrplot(cor(mydata), method="number")

#find relevant features
highlyCorrelated <- findCorrelation(cor(mydata), cutoff=0.5)


mydata$class <- as.factor(mydata$class)

#find important features
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(class~., data=mydata, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


#RFE for feature selection
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(mydata[,1:8], mydata[,9], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))



#modelling approach
newdata <- mydata[,1:7]
newdata$class <- mydata$class

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(7)

#logistic regression
fit.glm <- train(class~., data=newdata, method="glm", trControl=control)
#decision trees
fit.cart <- train(class~., data=newdata, method="rpart", trControl=control)
#LDA
fit.lda <- train(class~., data=newdata, method="lda", trControl=control)
#svm
fit.svm <- train(class~., data=newdata, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(class~., data=newdata, method="rf", trControl=control)
#bagged cart
fit.treebag <- train(class~., data=newdata, method="treebag", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(class~., data=newdata, method="gbm", trControl=control)

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


