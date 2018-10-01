#remove old data
rm(list=ls())

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#load data
df <- read_csv(file.choose())

#create backup

#summary

#look for missing data

#fix missing data

#look at risk profile

#recode risk profile

#remove redundant columns



#create models

library(randomForest)

library(gbm)

#output for kaggle - https://www.kaggle.com/uciml/german-credit
