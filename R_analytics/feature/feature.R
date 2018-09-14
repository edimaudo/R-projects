#clear old data
rm(list=ls())

#load libraries
library(readxl)
library(caTools)
library(caret)
library(mlbench)
library(tidyverse)
library(dplyr)
library(tidyr)

#read data
df_1 <- read_excel(file.choose(),1)
df_2 <- read_excel(file.choose(),2)
df_3 <- read_excel(file.choose(),3)

df_1 <- df_1 %>%
  dplyr::mutate(`feature-3` = as.factor(`feature-3`))

df_cts <- df_1 %>%
  select(`feature-1`, `feature-2`)

df_category <- df_1 %>%
  select(`feature-3`)

df_category <- as.data.frame(df_category)

df_output <- as.factor(df_1$output)

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_cts <- as.data.frame(lapply(df_cts, normalize))

#create dummy variables
library(dummies)
df_category.new <- dummy.data.frame((df_category), sep = "_")

df_combine <- cbind(df_cts, df_category.new, df_output)

#remove features
#find which features are not important
set.seed(1)
# # calculate correlation matrix
correlationMatrix <- cor(df_combine[,1:11])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

#drop columns
df_combine <- df_combine[,-c(highlyCorrelated)]

#split data into test and train
sample <- sample.split(df_combine,SplitRatio = 0.75)
training <- subset(df_combine,sample ==TRUE)
test <- subset(df_combine, sample==FALSE)

#models
predictor <- training[,1:10]
predicted <- training[,11]
model_gbm<-train(predictor,predicted,method='gbm')
model_rf<-train(predictor,predicted,method='rf')
model_nnet<-train(predictor,predicted,method='nnet')

#Predictions
predictions<-predict.train(object=model_gbm,test,type="raw")
#table(predictions)
confusionMatrix(predictions,test[,11])

predictions_rf <- predict.train(object=model_rf,test,type="raw")
confusionMatrix(predictions_rf,test[,11])

predictions_nnet <-  predict.train(object=model_nnet,test,type="raw")
confusionMatrix(predictions_nnet,test[,11])