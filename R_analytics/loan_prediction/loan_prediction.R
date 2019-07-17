#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies','readxl',
              'cluster','factoextra','psy','lattice','nFactors','scales','NbClust')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df_train <- read.table(file.choose(),sep = ",",header = TRUE)#read.delim(file.choose(),delim,header = TRUE)
df_test <- read.table(file.choose(),sep = ",",header = TRUE)

#get summary
summary(df_train)
summary(df_test)

#remove missing data
df_train <- na.omit(df_train)
df_test <- na.omit(df_test)

#drop unncessary columns
df_train[1] <- NULL
df_test[1] <- NULL

#recode categorical data

