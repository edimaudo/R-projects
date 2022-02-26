rm(list=ls()) # remove old data
################
# packages
################
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 
              'caTools','dummies','readxl','grid','gridExtra',
              'cluster','factoextra','psy','lattice','nFactors','scales','NbClust')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

################
# Load data
################
df_train <- read.table("train.txt",sep = ",",header = TRUE)
df_test <- read.table("test.txt",sep = ",",header = TRUE)

#===============
# Summary
#===============
summary(df_train)
summary(df_test)

#===============
# Data clean up
#===============
df_train <- na.omit(df_train)
df_test <- na.omit(df_test)

# drop ID column
df_train[1] <- NULL
df_test[1] <- NULL

#################
# Model
################

#===============
# Approach 1
#===============
# Train
# df_train_cat <- df_train[,c(1,2,3,4,5,11)]
# df_train_cat_new <- dummy.data.frame(as.data.frame(df_train_cat), sep = "_")
# df_train_cts <- df_train[,c(6,7,8,9,10)]
# Loan_status <- as.factor(df_train$Loan_Status)
# df_train_new <- cbind(df_train_cat_new, df_train_cts,Loan_status)
# 
# # TEST
# df_test_cat <- df_test[,c(1,2,3,4,5,11)]
# df_test_cat_new <- dummy.data.frame(as.data.frame(df_test_cat), sep = "_")
# df_test_cts <- df_test[,c(6,7,8,9,10)]
# df_test_new <- cbind(df_test_cat_new, df_test_cts)

#===============
# Approach 2
#===============
# Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}
# normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_Target <- as.data.frame(df_train[,c(12)])
colnames(df_Target) <- c("Target")

# Train
df_cat <- df_train[,c(1,2,3,4,5,11)]
df_cts <- df_train[,c(6,7,8,9,10)]
df_cts <- as.data.frame(lapply(df_cts, normalize))
df_cat <- as.data.frame(lapply(df_cat, labelEncoder))
train <- cbind(df_cts,df_cat, df_Target)


# Test
df_cat <- df_test[,c(1,2,3,4,5,11)]
df_cts <- df_test[,c(6,7,8,9,10)]
df_cts <- as.data.frame(lapply(df_cts, normalize))
df_cat <- as.data.frame(lapply(df_cat, labelEncoder))
test <- cbind(df_cts,df_cat)

#===============
#model training
#===============
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
#glm
fit.glm <- train(as.factor(crime)~., data=train, method="glm",family=binomial(),
                 metric = "Accuracy", trControl = control)
#random forest
fit.rf <- train(as.factor(crime)~., data=train, method="rf", 
                metric = "Accuracy", trControl = control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(as.factor(crime)~., data=train, method="gbm", 
                 metric = "Accuracy", trControl = control)
#svm
fit.svm <- train(as.factor(crime)~., data=train, method="svmRadial", 
                 metric = "Accuracy", trControl = control)
#nnet
fit.nnet <- train(as.factor(crime)~., data=train, method="nnet", 
                  metric = "Accuracy", trControl = control)
#naive
fit.naive <- train(as.factor(crime)~., data=train, 
                   method="naive_bayes", metric = "Accuracy", 
                   trControl = control)
#extreme gradient boosting
fit.xgb <- train(as.factor(crime)~., data=train, 
                 method="xgbTree", metric = "Accuracy", trControl = control)
#bagged cart
fit.bg <- train(as.factor(crime)~., data=train, 
                method="treebag", metric = "Accuracy", trControl = control)
#decision tree
fit.dtree <- train(as.factor(crime)~., data=train, 
                   method="C5.0", metric = "Accuracy", trControl = control)
#knn
fit.knn <- train(as.factor(crime)~., data=train, 
                 method="kknn", metric = "Accuracy", trControl = control)
#ensemble
fit.ensemble <- train(as.factor(crime)~., data=train, 
                      method="nodeHarvest", metric = "Accuracy", trControl = control)

stopCluster(cl)

#===============
#compare models
#===============
results <- resamples(list(randomforest = fit.rf, 
                          `gradient boost` = fit.gbm, 
                          `support vector machine` = fit.svm,
                          baggedCart = fit.bg, 
                          neuralnetwork = fit.nnet,
                          xgboost = fit.xgb, 
                          logisticregression = fit.glm, 
                          `decision tree` = fit.dtree, 
                          `naive bayes` = fit.naive,
                          `ensemble` = fit.ensemble, 
                          `knn` = fit.knn))

summary(results)
#===============
# boxplot comparison
#===============
bwplot(results)
#===============
# Dot-plot comparison
#===============
dotplot(results)

#===============
# Model accuracy
#===============
#mean(predicted.classes == test$crime)

#===============
# Make predictions
#===============
predicted.classes <- fit.knn %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = test$crime, mode = "everything")
output

#===============
#Variable Importance
#===============

caret::varImp(fit.rf)

#===============
# Confusion Matrix
#===============
output2 <- as.data.frame(output$table)
colnames(output2) <- c("Predicted",'Actual',"Freq")
cm_d_p <-  ggplot(data =output2, aes(x = Predicted , y =  Actual, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 8) +
  theme_light() +
  guides(fill=FALSE) 

cm_d_p