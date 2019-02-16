#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools')
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
#updated modeling
#-----------------
Output <- df$Output

df$Diseases <- as.factor(df$Diseases)

df_categorical <- df %>%
  dplyr::mutate(Diseases = as.factor(Diseases), Accidents = as.factor(Accidents), 
                Surgical_intervention = as.factor(Surgical_intervention), 
                High_fevers_in_last_year = as.factor(High_fevers_in_last_year),
                Smoking_Habit = as.factor(Smoking_Habit)) %>%
  dplyr::select(Diseases,Accidents,Surgical_intervention,High_fevers_in_last_year,Smoking_Habit)

df_continuous <- df %>%
  dplyr::select(Season ,Age,`Frequency of alcohol consumption`,Number_of_hours_spent_sitting)

#one hot encode columns
library(dummies)
df_category.new <- dummy.data.frame(df_categorical, sep = "_")

#normalize columns
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_continuous <- as.data.frame(lapply(df_continuous, normalize))

#combine data
df_new <- cbind(df_category.new,df_continuous,Output)

library(corrplot)
#update model by remove redundant columns
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:16])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

#drop columns
df_new <- df_new[,-c(highlyCorrelated)]

#create models
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#logistic regression
fit.glm <- train(Output~., data=df_new, method="glm", trControl=control)
#svm
fit.svm <- train(Output~., data=df_new, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(Output~., data=df_new, method="rf", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(Output~., data=df_new, method="gbm", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(logistic = fit.glm, svm = fit.svm, randomforest = fit.rf, gradboost = fit.gbm))
summary(results)

#seems like our initial model worked just fine - using svm





