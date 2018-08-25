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

#split data into categorical and non categorical data
mydata_predict <- mydata %>%
  select(Default)

mydata_not_category <- mydata %>%
  select(duration, amount, age)

#normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

mydata_not_category <- as.data.frame(lapply(mydata_not_category, normalize))

mydata_category <- as.data.frame(mydata[, !names(mydata) %in% colnames(mydata_not_category)]) 
mydata_category[,1] <- NULL

#one hot encoding
library(dummies)
mydata_category.new <- dummy.data.frame(mydata_category, sep = ".")

#combine data
df <- cbind(mydata_category.new, mydata_not_category, mydata_predict)

#for german credit
#check for balanced data
ggplot(data=mydata, aes(factor(Default))) + geom_bar() + theme_classic()
#lot more 0s than 1

#data balance
library(unbalanced)
n <- ncol(df)
output<- as.factor(df$Default)
input<- df[ ,-n]

#Balance the Dataset using ubSMOTE#
data<-ubBalance(X=input, Y=output, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
#View(data)
balancedData<-cbind(data$X,data$Y)

#check for balanced data
ggplot(data=balancedData, aes(factor(data$Y))) + geom_bar() + theme_classic()

#data <- NULL
#input <- NULL
#mydata_category.new <- NULL

#remove unnecessary features
#set seed
set.seed(1)
correlationMatrix <- cor(balancedData[,1:61])
# summarize the correlation matrix
#print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

corvalues <- c(highlyCorrelated)

balancedData.orig <- balancedData

#drop columns
balancedData <- as.data.frame(balancedData[,-corvalues])

#split data in test and train
library(caTools)
#set seed
set.seed(123)
sample <- sample.split(balancedData,SplitRatio = 0.75)
traindata <- subset(balancedData,sample ==TRUE)
testdata <- subset(balancedData, sample==FALSE)

#predictors
n1 <- ncol(traindata)
predictors <- traindata[,-n1]
#predict
predict <- traindata[,49]
#models
model_gbm <- train(predictors, predict, method='gbm', verbose=False)
predictions<-predict.train(object=model_gbm,testdata,type="raw")
table(predictions)
confusionMatrix(predictions,test[,22])
