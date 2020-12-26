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


df <- read_csv("customer_churn.csv")

#summary
summary(df)

#backup
df.orig <- df

#check for missing values
missing_data <- apply(df, 2, function(x) any(is.na(x))) 
print("missing data")
print(missing_data) # Total Charges

#remove NAs
df <- na.omit(df)

#convert from string to numeric
df <- as.data.frame(df, stringsAsFactors=F)

#check breakdown of Target variable
table(df$Churn)

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_customerID <- df[1]
df[1] <- NULL

#split data into different categories
df_cts <- df %>%
  select(tenure, TotalCharges)

df_cat <- df %>%
  select(-c(MonthlyCharges,TotalCharges, tenure, Churn))

df_churn <- df %>%
  select(Churn)

#recode data
df_cts <- as.data.frame(lapply(df_cts, normalize))
df_cat <- as.data.frame(lapply(df_cat, labelEncoder))

#combine data
df_new <- cbind(df_cts,df_cat,df_churn)

#create train and test data
set.seed(2020)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)


#model training
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs = FALSE)

#glm
fit.glm <- train(as.factor(Churn)~., data=train, method="multinom", metric = "Accuracy", trControl = control)
#random forest
fit.rf <- train(as.factor(Churn)~., data=train, method="rf", metric = "Accuracy", trControl = control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(as.factor(Churn)~., data=train, method="gbm", metric = "Accuracy", trControl = control)
#svm
fit.svm <- train(as.factor(Churn)~., data=train, method="svmRadial", metric = "Accuracy", trControl = control)
#nnet
fit.nnet <- train(as.factor(Churn)~., data=train, method="nnet", metric = "Accuracy", trControl = control)
#naive
fit.naive <- train(as.factor(Churn)~., data=train, method="naive_bayes", metric = "Accuracy", 
                   trControl = control)
#extreme gradient boosting
fit.xgb <- train(as.factor(Churn)~., data=train, method="xgbTree", metric = "Accuracy", trControl = control)
#bagged cart
fit.bg <- train(as.factor(Churn)~., data=train, method="treebag", metric = "Accuracy", trControl = control)
#decision tree
fit.dtree <- train(as.factor(Churn)~., data=train, method="C5.0", metric = "Accuracy", trControl = control)
#knn
fit.knn <- train(as.factor(Churn)~., data=train, method="kknn", metric = "Accuracy", trControl = control)

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

#ROC outcome
dotplot(results, metric = "ROC")

# Make predictions
predicted.classes <- fit.xgb %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = test$Churn, mode = "everything")

#feature importance
caret::varImp(fit.Xgb)

#plot confusion matrix
output2 <- as.data.frame(output$table)
colnames(output2) <- c("Predicted",'Actual',"Freq")
cm_d_p <-  ggplot(data =output2, aes(x = Predicted , y =  Actual, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 8) +
  theme_light() +
  guides(fill=FALSE) 

#plot output
cm_d_p 


#AUC dtree
aucdtree <- roc(as.numeric(test$Churn), as.numeric(fit.dtree),  ci=TRUE)
plot(aucdtree, ylim=c(0,1), print.thres=TRUE, 
     main=paste('AUC:',round(auxdtree$auc[[1]],3)),col = 'blue')


# #compare ROC curves 
# plot(aucrf, ylim=c(0,1), main=paste('ROC Comparison : 
#                                     RF(blue),decision tree(green),Logistic(Red)'),col = 'blue')
# par(new = TRUE)
# plot(aucglm,col = "red")
# par(new = TRUE)
# plot(aucdt,col="green")