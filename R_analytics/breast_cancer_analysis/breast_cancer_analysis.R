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
df.orig <- df

#check for missing values
missing_data <- apply(df, 2, function(x) any(is.na(x))) 
print("missing data")
print(missing_data)

#remove NA
df <- na.omit(df)

#convert from string to numeric
df <- as.data.frame(df, stringsAsFactors=F)

#correlation
corinfo <- df[,2:11]
corrplot(cor(corinfo), method="number")

id_info <- data[1]

df[1] <- NULL #drop the first column

#create train and test data
set.seed(2020)
sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

#model training
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs = FALSE)
#glm
fit.glm <- train(as.factor(Diagnosis)~., data=train, method="glm", metric = "Accuracy", trControl = control)
#random forest
fit.rf <- train(as.factor(Diagnosis)~., data=train, method="rf", metric = "Accuracy", trControl = control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(as.factor(Diagnosis)~., data=train, method="gbm", metric = "Accuracy", trControl = control)
#svm
fit.svm <- train(as.factor(Diagnosis)~., data=train, method="svmRadial", metric = "Accuracy", trControl = control)
#nnet
fit.nnet <- train(as.factor(Diagnosis)~., data=train, method="nnet", metric = "Accuracy", trControl = control)
#naive
fit.naive <- train(as.factor(Diagnosis)~., data=train, method="naive_bayes", metric = "Accuracy", 
                   trControl = control)
#extreme gradient boosting
fit.xgb <- train(as.factor(Diagnosis)~., data=train, method="xgbTree", metric = "Accuracy", trControl = control)
#bagged cart
fit.bg <- train(as.factor(Diagnosis)~., data=train, method="treebag", metric = "Accuracy", trControl = control)
#decision tree
fit.dtree <- train(as.factor(Diagnosis)~., data=train, method="C5.0", metric = "Accuracy", trControl = control)
#knn
fit.knn <- train(as.factor(Diagnosis)~., data=train, method="kknn", metric = "Accuracy", trControl = control)


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
#feature importance
varImpPlot(model_rf_df$finalModel)

#plot confusion matrix
output2 <- as.data.frame(output$table)
colnames(output2) <- c("Predicted",'Actual',"Freq")
cm_d_p <-  ggplot(data =output2, aes(x = Predicted , y =  Actual, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 8) +
  theme_light() +
  guides(fill=FALSE) 

aucxgb <- roc(as.numeric(test$class), as.numeric(fit.xgb),  ci=TRUE)
plot(aucxgb, ylim=c(0,1), print.thres=TRUE, 
     main=paste('AUC:',round(auxgb$auc[[1]],3)),col = 'blue')


#AUC
aucrf <- roc(as.numeric(testSplit$class), as.numeric(predrf),  ci=TRUE)
plot(aucrf, ylim=c(0,1), print.thres=TRUE, 
     main=paste('Random Forest AUC:',round(aucrf$auc[[1]],3)),col = 'blue')

#compare ROC curves 
plot(aucrf, ylim=c(0,1), main=paste('ROC Comparison : 
                                    RF(blue),decision tree(green),Logistic(Red)'),col = 'blue')
par(new = TRUE)
plot(aucglm,col = "red")
par(new = TRUE)
plot(aucdt,col="green")