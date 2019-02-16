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
df$Output <- recode_factor(df$Output, "N" = "1","O" = "2")

#--------------------
#initial prediction
#--------------------
set.seed(123)
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#logistic regression
fit.glm <- train(Output~., data=df, method="glm", trControl=control)
#svm
fit.svm <- train(Output~., data=df, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(Output~., data=df, method="rf", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(Output~., data=df, method="gbm", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(logistic = fit.glm, svm = fit.svm, randomforest = fit.rf, gradboost = fit.gbm))
summary(results)

#-----------------
#updated prediction
#-----------------

#one hot encode columns
library(dummies)

df_categorical <- df[c(3,4,5,6,8),]

#normalize columns


#create models

#check best model

#update model by remove redundant columns

#create models

#