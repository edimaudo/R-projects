#remove all data
rm(list=ls())

#libraries
library(readxl)
library(tidyverse)
library(caret)
library(mlbench)
library(data.table)

#read data
df = as.data.frame(read_excel(file.choose())) #excel file

#view data
glimpse(df)

#check for missing data
apply(df, 2, function(x) any(is.na(x))) #no missing data

#split data into training and test
set.seed(123)
train<-sample_frac(df, 0.8)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-df[-sid,]

#check for correlations
library(corrplot)
corrplot(cor(train), method = "number")

#build initial model


#check for importance variables

#build new model

#check accuracy and AIC

#test using test data

