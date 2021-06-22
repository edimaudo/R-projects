# Classify bank loans

#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# Packages
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','readxl','doParallel')

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

#===================
# Check for imbalance
#===================

#tweak for sampling
#check for imbalance
#print(table(df_new$crime))
# #weight due to 
# model_weights <- ifelse(train$crime == "Assault",
#                         (1/table(train$crime)[1]) * 0.5,
#                         (1/table(train$crime)[2]) * 0.5)

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}

df_cts <- df[,c(1,11,12,13,14,16,17,18,19,20)]
df_cts <- as.data.frame(lapply(df_cts, normalize))

#===================
# Data modeling
#===================
sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)


#model training
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
# Prediction model
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
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
fit.dtree <- train(as.factor(crime)~., data=train, 
                   method="C5.0", metric = "Accuracy", trControl = control)
#knn
fit.knn <- train(as.factor(crime)~., data=train, 
                 method="kknn", metric = "Accuracy", trControl = control)
#ensemble
fit.ensemble <- train(as.factor(crime)~., data=train, 
                      method="nodeHarvest", metric = "Accuracy", trControl = control)

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
#logistic regression
logistic_regression_model <- nnet::multinom(crime ~ ., 
                                            data = crimes_data_model2 %>% 
                                              select(Year, Age,`Job_House Wife`,`Job_Maid`,
                                                     `Marital Status_Single`,
                                                     Nationality_UAE, Target))
summary(logistic_regression_model)
#use of test data
test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$Output)

#===================
# Predictions
#===================
predicted.classes <- fit.knn %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = test$crime, mode = "everything")
output
#===================
#other metrics
#===================
caret::varImp(fit.rf)

#plot confusion matrix of selected option
library(ggplot2)     # to plot
library(gridExtra)   # to put more
library(grid)        # plot together

# plotting the matrix
output2 <- as.data.frame(output$table)
colnames(output2) <- c("Predicted",'Actual',"Freq")
cm_d_p <-  ggplot(data =output2, aes(x = Predicted , y =  Actual, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 8) +
  theme_light() +
  guides(fill=FALSE) 

cm_d_p















