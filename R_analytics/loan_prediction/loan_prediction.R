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


#combine train and test

#remove missing data

#drop unncessary columns

#recode categorical data

