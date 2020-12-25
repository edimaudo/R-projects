#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','pROC','grid')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_csv("breast-cancer-wisconsin.csv")

#summary
summary(df)

#back up data
data.orig <- data

#check for missing values

#remove NA
data <- na.omit(data)



#create train and test data
set.seed(2020)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#model training
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs = FALSE)

#glm
fit.glm <- train(as.factor(crime)~., data=train, method="multinom", metric = "Accuracy", trControl = control)
#random forest
fit.rf <- train(as.factor(crime)~., data=train, method="rf", metric = "Accuracy", trControl = control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(as.factor(crime)~., data=train, method="gbm", metric = "Accuracy", trControl = control)
#svm
fit.svm <- train(as.factor(crime)~., data=train, method="svmRadial", metric = "Accuracy", trControl = control)
#nnet
fit.nnet <- train(as.factor(crime)~., data=train, method="nnet", metric = "Accuracy", trControl = control)
#naive
fit.naive <- train(as.factor(crime)~., data=train, method="naive_bayes", metric = "Accuracy", 
                   trControl = control)
#extreme gradient boosting
fit.xgb <- train(as.factor(crime)~., data=train, method="xgbTree", metric = "Accuracy", trControl = control)
#bagged cart
fit.bg <- train(as.factor(crime)~., data=train, method="treebag", metric = "Accuracy", trControl = control)
#decision tree
fit.dtree <- train(as.factor(crime)~., data=train, method="C5.0", metric = "Accuracy", trControl = control)
#knn
fit.knn <- train(as.factor(crime)~., data=train, method="kknn", metric = "Accuracy", trControl = control)


stopCluster(cl)

#------------------
#compare models
#------------------
results <- resamples(list(randomforest = fit.rf, 
                          `gradient boost` = fit.gbm, 
                          `support vector machine` = fit.svm,
                          baggedCart = fit.bg, 
                          neuralnetwork = fit.nnet,
                          xgboost = fit.xgb, 
                          logisticregression = fit.glm, 
                          `decision tree` = fit.dtree, 
                          `naive bayes` = fit.naive))

summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)


# Make predictions
predicted.classes <- fit.xgb %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = test$crime, mode = "everything")

caret::varImp(fit.xgb)



#plot confusion matrix
output2 <- as.data.frame(output$table)
colnames(output2) <- c("Predicted",'Actual',"Freq")
cm_d_p <-  ggplot(data =output2, aes(x = Predicted , y =  Actual, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 8) +
  theme_light() +
  guides(fill=FALSE) 