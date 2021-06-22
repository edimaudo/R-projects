#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','readxl')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Load data
df <- read.csv("bankloan.csv")

# understand data
summary(df)


glimpse(df)

introduce(df)

plot_bar(df)

#drop id
df['id'] <- NULL

#look for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)



#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


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

#drop id

#split into train and test
sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

#--------------------
#initial prediction
#--------------------
set.seed(123)
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#logistic regression
fit.glm <- train(Output~., data=train, method="glm", trControl=control)
#svm
fit.svm <- train(Output~., data=train, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(Output~., data=train, method="rf", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(Output~., data=train, method="gbm", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(logistic = fit.glm, svm = fit.svm, 
                          randomforest = fit.rf, gradboost = fit.gbm))
summary(results)

#apply model on test data
test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$Output)

#create models
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#logistic regression
fit.glm <- train(Output~., data=train, method="glm", trControl=control)
#svm
fit.svm <- train(Output~., data=train, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(Output~., data=train, method="rf", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(Output~., data=train, method="gbm", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(logistic = fit.glm, svm = fit.svm, randomforest = fit.rf, gradboost = fit.gbm))
summary(results)

#use of test data
test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$Output)


#regression modelling

fit <- lm(Price ~., data=training)
summary(fit)
plot(Price ~., data=training)
plot(residuals(fit) ~ ., data=training)

pred1 <- predict(fit, newdata = test)
rmse <- sqrt(sum((exp(pred1) - test$medv)^2)/length(test$medv))
c(RMSE = rmse, R2=summary(fit)$r.squared)




#data to keep
keep <- c("age","day","balance","campaign","pdays","job_entrepreneur","job_blue-collar","job_unknown","job_retired","job_admin.","job_services",          "job_self-employed","job_unemployed","job_housemaid","job_student","marital_single","marital_divorced","education_secondary","education_primary","default_yes","housing_no","loan_yes","contact_cellular","contact_telephone","month_jun","month_jul", "month_aug","month_oct","month_nov","month_dec","month_jan","month_feb","month_mar","month_apr","month_sep","poutcome_failure","poutcome_other","poutcome_success","y_yes")          
final <- dummy_data[keep]

#split data
set.seed(123)

sample <- sample.split(final,SplitRatio = 0.75)
training <- subset(final,sample ==TRUE)
test <- subset(final, sample==FALSE)