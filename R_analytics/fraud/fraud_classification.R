#===================
# Fraud classification
#===================
rm(list = ls()) #clear environment
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','grid')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# load data
df <- read.csv("FraudData.csv")

#back up 
df.backup <- df

#===================
# Understand data
#===================
glimpse(df)

summary(df)

#look for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_cts <- df %>%
  select(X, amount, age)

df_cat <- df %>%
  select(category, gender, city, state)

df_fraud <- df %>%
  select(fraud)

#combine data
df_cts <- as.data.frame(lapply(df_cts, normalize))
df_cat <- as.data.frame(lapply(df_cat, labelEncoder))
df_new <- cbind(df_cts,df_cat,df_fraud)

#create train and test data
set.seed(2021)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#model training
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# tweak for sampling
# CHECK FOR IMBLANANCE
table(df$fraud)

# #weight due to 
model_weights <- ifelse(train$fraud == 0,
                         (1/table(train$fraud)[1]) * 0.5,
                         (1/table(train$fraud)[2]) * 0.5)
# cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=5, classProbs = FALSE)
# #glm
fit.glm <- train(as.factor(fraud)~., data=train, method="glm",family=binomial(),
                 metric = "Accuracy", trControl = control, weights = model_weights)
#random forest
fit.rf <- train(as.factor(fraud)~., data=train, method="rf",
                metric = "Accuracy", trControl = control, weights = model_weights)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(as.factor(fraud)~., data=train, method="gbm",
                 metric = "Accuracy", trControl = control, weights = model_weights)
#svm
fit.svm <- train(as.factor(fraud)~., data=train, method="svmRadial",
                 metric = "Accuracy", trControl = control, weights = model_weights)
#nnet
fit.nnet <- train(as.factor(fraud)~., data=train, method="nnet",
                  metric = "Accuracy", trControl = control, weights = model_weights)
#naive
fit.naive <- train(as.factor(fraud)~., data=train,
                   method="naive_bayes", metric = "Accuracy",
                   trControl = control, weights = model_weights)
#extreme gradient boosting
fit.xgb <- train(as.factor(fraud)~., data=train,
                 method="xgbTree", metric = "Accuracy",
                 trControl = control, weights = model_weights)
#bagged cart
fit.bg <- train(as.factor(fraud)~., data=train,
                method="treebag", metric = "Accuracy",
                trControl = control, weights = model_weights)
#decision tree
fit.dtree <- train(as.factor(fraud)~., data=train,
                   method="C5.0", metric = "Accuracy",
                   trControl = control, weights = model_weights)
#knn
fit.knn <- train(as.factor(fraud)~., data=train,
                 method="kknn", metric = "Accuracy",
                 trControl = control, weights = model_weights)
#ensemble
fit.ensemble <- train(as.factor(fraud)~., data=train,
                      method="nodeHarvest", metric = "Accuracy",
                      trControl = control, weights = model_weights)
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
                         # `decision tree` = fit.dtree, 
                          `naive bayes` = fit.naive
                          #`ensemble` = fit.ensemble, 
                          #`knn` = fit.knn
                         ))

summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

# Model accuracy
mean(predicted.classes == test$fraud)

# test data accuracy
# Make predictions
predicted.classes <- fit.xgb %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = as.factor(test$fraud), mode = "everything")
output

#other metrics
caret::varImp(fit.xgb)

# plotting the matrix
output2 <- as.data.frame(output$table)
colnames(output2) <- c("Predicted",'Actual',"Freq")
cm_d_p <-  ggplot(data =output2, aes(x = Predicted , y =  Actual, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 8) +
  theme_light() +
  guides(fill=FALSE) 

cm_d_p