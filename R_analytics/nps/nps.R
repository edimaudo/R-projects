#nps - net promoter score

#remove all data
rm(list=ls())

#libraries
library(readxl)
library(tidyverse)
library(caret)
library(mlbench)
library(data.table)
library(corrplot)
library(pastecs)

#load nps data
df <- read.csv(file.choose(), sep=";")

#view data
glimpse(df)

#check for missing data
apply(df, 2, function(x) any(is.na(x))) #no missing data

#set seed for reproducability
set.seed(123)

#descriptive statistics
summary(df)
stat.desc(df) 

# Contigency analysis 
chisq.test(table(df$BU, df$REC))
chisq.test(table(df$Region, df$REC))


# multiple linear regression - dependdent variable is REC
# outliers, non normality
# note - use either BU or PRODUCT but not both
# note -  check linear regression assumptions
# - split data into test and train
# - initial model using train data
# - find important variables
# - build new model and test
# - get AIC, accuracy