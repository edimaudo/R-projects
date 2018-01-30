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

#load data
training <- read.csv(file.choose(), sep = ",")
test <- read.csv(file.choose(), sep = ",")

#explore
glimpse(training)
glimpse(test)

names(training)

#remove the first columns
training[1] <- NULL
test[1] <- NULL

#backup 
training.orig <- training
test.orig <- test


#remove na since data imputation would give odd results
training <- na.omit(training)



#modeling
#logistic regression


#decision trees

#random forest

#ANN

#support vector machines


#compare models

#model AUC