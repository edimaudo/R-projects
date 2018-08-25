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

#set seed
set.seed(1)

#check for balanced data
ggplot(data=mydata, aes(factor(Default))) + geom_bar() + theme_classic()
#lot more 0s than 1

#data balance

#split data into categorical and non categorical data

my_data_category <- mydata %>%
  select()

mydata_predict <- mydata %>%
  select()

mydata_not_category <- mydata %>%
  select()

#normalization function

#normalize cts variables

#remove unnecessary features

#combine category, non category and predict

#split data in test and train