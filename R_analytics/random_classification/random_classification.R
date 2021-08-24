# Classify Y value

#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# Packages
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies",'dplyr',
              'plyr','mlbench','caTools','doParallel')

# Load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


# Load data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# back up 
train.backup <- train 
test.backup <- test

glimpse(train)
glimpse(test)

#look for missing data
missing_data <- apply(train, 2, function(x) any(is.na(x)))
print(missing_data)

missing_data1 <- apply(test, 2, function(x) any(is.na(x)))
print(missing_data1)

# columns to drop
cols_remove <- c('x5','x11','x14','x16','x22','x24','x26','x30','x33','x38','x41','x42','x44','x45',
                 'x49','x52','x54','x55','x57','x61','x63','x64','x67','x68','x74','x75','x76','x77',
                 'x78','x79','x80','x83','x85','x86','x88','x89','x91','x92','x94','x95','x96','x99')


train <- train %>%
  select(-cols_remove)

test <- test %>%
  select(-cols_remove)

glimpse(train)


#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}

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

# update x31 if needed

# update x39 if needed

# update x60 if needed

# update x65 if needed

# update x93 if needed

# transform data

#===================
# Calculate correlation
#===================
correlationMatrix <- cor(train)
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
model_weights <- ifelse(train$default == "1",
                        (1/table(train$default)[1]) * 0.5,
                        (1/table(train$default)[2]) * 0.5)

#model training
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# Prediction model
set.seed(123)
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# control <- trainControl(method="repeatedcv", number=10, repeats=5, classProbs = FALSE)

# models
#glm
fit.glm <- train(as.factor(default)~., data=train, method="glm",family=binomial(),
                 metric = "Accuracy", trControl = control, weights = model_weights)
#svm
fit.svm <- train(as.factor(default)~., data=train, method="svmRadial", 
                 metric = "Accuracy", trControl = control, weights = model_weights)
#random forest
fit.rf <- train(as.factor(default)~., data=train, method="rf",
                metric = "Accuracy", trControl = control, weights = model_weights)

#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(as.factor(default)~., data=train, method="gbm",
                 metric = "Accuracy", trControl = control, weights = model_weights)
#nnet
fit.nnet <- train(as.factor(default)~., data=train, method="nnet", 
                  metric = "Accuracy", trControl = control, weights = model_weights)
#naive
fit.naive <- train(as.factor(default)~., data=train, method="naive_bayes", 
                   metric = "Accuracy", trControl = control, weights = model_weights)

#extreme gradient boosting
fit.xgb <- train(as.factor(default)~., data=train, method="xgbTree", 
                 metric = "Accuracy", trControl = control, weights = model_weights)

#bagged cart
fit.bg <- train(as.factor(default)~., data=train, method="treebag", 
                metric = "Accuracy", trControl = control, weights = model_weights)

#decision tree
fit.dtree <- train(as.factor(default)~., data=train, method="C5.0", 
                   metric = "Accuracy", trControl = control,weights = model_weights)

#knn
fit.knn <- train(as.factor(default)~., data=train, method="kknn", 
                 metric = "Accuracy", trControl = control,weights = model_weights)
#ensemble
fit.ensemble <- train(as.factor(default)~., data=train, method="nodeHarvest", 
                      metric = "Accuracy", trControl = control,weights = model_weights)

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
                          #`decision tree` = fit.dtree, 
                          `naive bayes` = fit.naive,
                          `ensemble` = fit.ensemble, 
                          `knn` = fit.knn))

summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

#===================
# Predictions
#===================