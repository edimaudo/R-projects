#remove old data
rm(list=ls())

#load libraries
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools', 
              'MASS','Metrics','randomForest','lars','xgboost','Matrix','methods', 'data.table')

#load libraries
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
train <- fread(file.choose())





#load test data
test <- fread(file.choose())

#load sample submission
sample_submission <- read_csv(file.choose())