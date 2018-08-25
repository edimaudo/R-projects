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

