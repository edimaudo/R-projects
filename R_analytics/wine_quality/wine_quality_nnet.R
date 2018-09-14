#clear old data
rm(list=ls())

#libraries
library(tidyverse)
library(neuralnet)
library(caret)
library(mlbench)
library(caTools)
library(corrplot)
library(ggplot2)

#load data
df <- read.csv(file.choose(), sep=";")

df_predict <- as.factor(df$quality)

df$quality <- NULL

#normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df <- as.data.frame(lapply(df, normalize))

df <- cbind(df,df_predict)
df <- as.data.frame(df)

#find features
#find which features are not important
set.seed(1)
# # calculate correlation matrix
correlationMatrix <- cor(df[,1:11])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

#drop columns
df <- df[,-c(highlyCorrelated)]

#split data into train and test
set.seed(1)
sample <- sample.split(df,SplitRatio = 0.75)
training <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

nn <- neuralnet(df_predict~ alcohol + sulphates, data = training, linear.output = FALSE, 
                err.fct = 'ce', likelihood = TRUE)