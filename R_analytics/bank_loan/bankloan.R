# Classify bank loans

#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# Packages
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','readxl','gridExtra','grid')

# Load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#===================
# Load data
#===================
df <- read.csv("bankloan.csv")

#back up 
df.backup <- df

#===================
# Understand data
#===================
glimpse(df)

summary(df)

#drop id
df['id'] <- NULL

#look for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

#drop na
df <- na.omit(df)

#===================
# Calculate correlation
#===================
correlationMatrix <- cor(df)
corrplot(correlationMatrix,method='number')
# # summarize the correlation matrix
print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}


df <- as.data.frame(lapply(df, normalize))

#===================
# Data modeling
#===================
sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

#===================
# Check for imbalance
#===================
print(table(df$default))
#tweak for sampling

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
predicted.classes <- fit.rf %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = test$default, mode = "everything")
output

#===================
#other metrics
#===================
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















