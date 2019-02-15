#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools', 
              'MASS','Metrics','randomForest','lars','xgboost','Matrix','methods','readxl')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
#load data
df <- read.table(file.choose(),sep=",") #"breast-cancer-wisconsin.data",header=F,sep=",",stringsAsFactors=F)

#summary statisitcs
summary(df)

#rename the columns

#recode columns

#set seed

#split into train and test

#create models

#check best model

#update model by remove redundant columns

#create models

#