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
df <- read.table(file.choose(),sep=",")

#summary statisitcs
summary(df)

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#rename the columns
colnames(df) <- c('Season','Age','Diseases','Accidents','Surgical_intervention','High_fevers_in_last_year',
                  'Frequency of alcohol consumption','Smoking_Habit','Number_of_hours_spent_sitting','Output')

#recode columns

#set seed

#split into train and test

#create models

#check best model

#update model by remove redundant columns

#create models

#