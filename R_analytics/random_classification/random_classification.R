# Classify Y value

#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

#=================
# Packages
#=================
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies",'dplyr',
              'plyr','mlbench','caTools','doParallel')

# Load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#=================
# Load data
#=================
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# back up 
train.backup <- train 
test.backup <- test

#look for missing data
missing_data <- apply(train, 2, function(x) any(is.na(x)))
print(missing_data)

missing_data1 <- apply(test, 2, function(x) any(is.na(x)))
print(missing_data1)

# columns to drop
cols_remove <- c('x5','x11','x14','x16','x7','x22','x24','x26','x30','x33','x38','x41','x42','x44','x45',
                 'x49','x52','x54','x55','x57','x61','x63','x64','x67','x68','x74','x75','x76','x77',
                 'x78','x79','x80','x83','x85','x86','x88','x89','x91','x92','x94','x95','x96','x99')

train <- train %>%
  select(-cols_remove)

test <- test %>%
  select(-cols_remove)

# update x3 if needed
train$x3 <- revalue(train$x3, c('Mon'="Monday","Tue"="Tuesday","Wed"="Wednesday","Thur"="Thursday",
                                "Fri"="Friday","Sat"="Saturday","Sun"="Sunday",
                                "Monday"="Monday","Tuesday"="Tuesday","Wednesday"="Wednesday",
                                "Thursday"="Thursday","Friday"="Friday","Saturday"="Saturday",
                                "Sunday"="Sunday"))
test$x3 <- revalue(test$x3, c('Mon'="Monday","Tue"="Tuesday","Wed"="Wednesday","Thur"="Thursday",
                                "Fri"="Friday","Sat"="Saturday","Sun"="Sunday",
                                "Monday"="Monday","Tuesday"="Tuesday","Wednesday"="Wednesday",
                                "Thursday"="Thursday","Friday"="Friday","Saturday"="Saturday",
                                "Sunday"="Sunday"))

# update x19 if needed
train$x19 <- substring(train$x19,2)
test$x19 <- substring(test$x19,2)

train$x19 <- as.double(train$x19)
test$x19 <- as.double(test$x19)

#===================
# transform data
#===================

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}

y <- train$y

train_cat <- train %>%
  select(c("x3","x31","x39",'x60','x65','x93'))

train_cts <- train %>%
  select(-c("y","x3","x31","x39",'x60','x65','x93'))

train_cts <- as.data.frame(lapply(train_cts, normalize))
train_cat <- as.data.frame(lapply(train_cat, labelEncoder))
train_df <- cbind(train_cts,train_cat,y)

test_cat <- test %>%
  select(c("x3","x31","x39",'x60','x65','x93'))

test_cts <- test %>%
  select(-c("x3","x31","x39",'x60','x65','x93'))

test_cts <- as.data.frame(lapply(test_cts, normalize))
test_cat <- as.data.frame(lapply(test_cat, labelEncoder))
test_df <- cbind(test_cts, test_cat)


#===================
# Calculate correlation
#===================
correlationMatrix <- cor(train_df)
corrplot(correlationMatrix,method='number')
# # summarize the correlation matrix
print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

#================
# build model
#================

# #weight due to 
model_weights <- ifelse(train_df$y == "1",
                        (1/table(train_df$default)[1]) * 0.5,
                        (1/table(train_df$default)[2]) * 0.5)

#model training
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# Prediction model
set.seed(123)
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# models
#glm
fit.glm <- train(as.factor(y)~., data=train_df, method="glm",family=binomial(),
                 metric = "Accuracy", trControl = control)
#svm
fit.svm <- train(as.factor(y)~., data=train_df, method="svmRadial", 
                 metric = "Accuracy", trControl = control)
#random forest
fit.rf <- train(as.factor(y)~., data=train_df, method="rf",
                metric = "Accuracy", trControl = control)

#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(as.factor(y)~., data=train_df, method="gbm",
                 metric = "Accuracy", trControl = control)
#nnet
fit.nnet <- train(as.factor(y)~., data=train_df, method="nnet", 
                  metric = "Accuracy", trControl = control)
#naive
fit.naive <- train(as.factor(y)~., data=train_df, method="naive_bayes", 
                   metric = "Accuracy", trControl = control)

#extreme gradient boosting
fit.xgb <- train(as.factor(y)~., data=train_df, method="xgbTree", 
                 metric = "Accuracy", trControl = control)

#bagged cart
fit.bg <- train(as.factor(y)~., data=train_df, method="treebag", 
                metric = "Accuracy", trControl = control)

#decision tree
fit.dtree <- train(as.factor(y)~., data=train_df, method="C5.0", 
                   metric = "Accuracy", trControl = control)

#knn
fit.knn <- train(as.factor(y)~., data=train_df, method="kknn", 
                 metric = "Accuracy", trControl = control)
#ensemble
fit.ensemble <- train(as.factor(y)~., data=train_df, method="nodeHarvest", 
                      metric = "Accuracy", trControl = control)

cl <- makePSOCKcluster(4)
registerDoParallel(cl)
stopCluster(cl)

#===================
#compare models
#===================
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
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

# Model accuracy
#mean(predicted.classes == test$crime)
#test data accuracy

#===================
# Predictions
#===================
predicted.classes <- fit.knn %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = test$crime, mode = "everything")
output


# variable importance
caret::varImp(fit.rf)


#plot confusion matrix of selected option
output2 <- as.data.frame(output$table)
colnames(output2) <- c("Predicted",'Actual',"Freq")
cm_d_p <-  ggplot(data =output2, aes(x = Predicted , y =  Actual, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 8) +
  theme_light() +
  guides(fill=FALSE) 

cm_d_p