#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','readxl')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
#load data
train <- read_excel(file.choose(), "Training_Data")
test <- read_excel(file.choose(), "Test_Data")

glimpse(train)

#summary statisitcs
summary(train)

#normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

UNS_train <- train$UNS


#recode
UNS_train <- recode_factor(UNS_train, "very_low"="1", "Low"= "2", "Middle" = "3", "High"="4")
df_train <- train[,1:5]
UNS <- recode_factor(UNS, "Very Low"="1", "Low"= "2", "Middle" = "3", "High"="4")
df_train <- as.data.frame(lapply(df_train, normalize))


train <- cbind(df_train, UNS_train)


resultdata <- function(control, train){
  set.seed(123)
  #decision trees
  fit.cart <- train(class~., data=train, method="rpart", trControl=control)
  #LDA
  fit.lda <- train(class~., data=train, method="lda", trControl=control)
  #svm
  fit.svm <- train(class~., data=train, method="svmRadial", trControl=control)
  #random forest
  fit.rf <- train(class~., data=train, method="rf", trControl=control)
  #bagged cart
  fit.treebag <- train(class~., data=train, method="treebag", trControl=control)
  #boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
  fit.gbm <- train(class~., data=train, method="gbm", trControl=control)
  
  #------------------
  #compare models
  #------------------
  results <- resamples(list(cart = fit.cart, lda = fit.lda,
                            svm = fit.svm, randomforest = fit.rf, 
                            baggedcart = fit.treebag, gradboost = fit.gbm))
  
  return (results)
}

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
results <- resultdata(control, train)

summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

#use test data
fit.gbm <- train(UNS_train~., data=train, method="gbm", trControl=control)
UNS_test <- test$UNS
df_test <- test[,1:5]
df_test <- as.data.frame(lapply(df_test, normalize))
test <- cbind(df_test, UNS_test)
test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$UNS_test)