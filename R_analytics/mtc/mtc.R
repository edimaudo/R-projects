#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','readxl','grid','gridExtra')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#===================
## Load Data
#===================
df <- read_excel("mtc.xlsx")

#===================
# Data Overview
#===================

# Summary
summary(df)

# Check for missing variables
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

# Check for for imbalance
table(df$label)

# drop missing values
df <- na.omit(df)

#=================
# Modeling
#=================

# Select columns
df <- df %>%
  select(CANTIDAD,CANTIDAD_FISICA,CODIGO_CERTIFICADO,NOMBRE_PROVEEDOR,NOMBRE_EQUIPO,
         MODELO, PARTIDA_ARANCELARIA,MARCA_EQUIPO,FACTURA,UM_FISICA_ID,label)

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_other <- df %>%
  select(UM_FISICA_ID,label)

df_cts <- df %>%
  select(CANTIDAD_FISICA)

df_cat<- df %>%
  select(CODIGO_CERTIFICADO,NOMBRE_PROVEEDOR,NOMBRE_EQUIPO,
         MODELO, PARTIDA_ARANCELARIA,MARCA_EQUIPO,FACTURA)

#combine data
df_cts <- as.data.frame(lapply(df_cts, normalize))
df_cat <- as.data.frame(lapply(df_cat, labelEncoder))
df_new <- cbind(df_cts,df_cat,df_other)

#create train and test data
set.seed(2020)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

# #weight due to 
model_weights <- ifelse(train$crime == 0,
                         (1/table(train$crime)[1]) * 0.5,
                         (1/table(train$crime)[2]) * 0.5)
# #cross fold validation
# control <- trainControl(method="repeatedcv", number=10, repeats=5, classProbs = FALSE)
# #glm
# fit.glm <- train(as.factor(crime)~., data=train, method="glm",family=binomial(),
#                  metric = "Accuracy", trControl = control, weights = model_weights)

#model training
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
#glm
fit.glm <- train(as.factor(label)~., data=train, method="glm",family=binomial(),
                 metric = "Accuracy", trControl = control)
#random forest
fit.rf <- train(as.factor(label)~., data=train, method="rf", 
                metric = "Accuracy", trControl = control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(as.factor(label)~., data=train, method="gbm", 
                 metric = "Accuracy", trControl = control)
#svm
fit.svm <- train(as.factor(label)~., data=train, method="svmRadial", 
                 metric = "Accuracy", trControl = control)
#nnet
fit.nnet <- train(as.factor(label)~., data=train, method="nnet", 
                  metric = "Accuracy", trControl = control)
#naive
fit.naive <- train(as.factor(label)~., data=train, 
                   method="naive_bayes", metric = "Accuracy", 
                   trControl = control)
#extreme gradient boosting
fit.xgb <- train(as.factor(label)~., data=train, 
                 method="xgbTree", metric = "Accuracy", trControl = control)
#bagged cart
fit.bg <- train(as.factor(label)~., data=train, 
                method="treebag", metric = "Accuracy", trControl = control)
#decision tree
fit.dtree <- train(as.factor(label)~., data=train, 
                   method="C5.0", metric = "Accuracy", trControl = control)
#knn
fit.knn <- train(as.factor(label)~., data=train, 
                 method="kknn", metric = "Accuracy", trControl = control)
#ensemble
fit.ensemble <- train(as.factor(label)~., data=train, 
                      method="nodeHarvest", metric = "Accuracy", trControl = control)

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
                          `naive bayes` = fit.naive,
                          `ensemble` = fit.ensemble, 
                          `knn` = fit.knn))

summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

# Model accuracy
#mean(predicted.classes == test$label)
#test data accuracy
# Make predictions
predicted.classes <- fit.knn %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = test$label, mode = "everything")
output
#other metrics

caret::varImp(fit.rf)

#confusion matrix output
#write.csv(as.data.frame(output$byClass),"output.csv")

#plot confusion matrix of selected option


# plotting the matrix
output2 <- as.data.frame(output$table)
colnames(output2) <- c("Predicted",'Actual',"Freq")
cm_d_p <-  ggplot(data =output2, aes(x = Predicted , y =  Actual, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 8) +
  theme_light() +
  guides(fill=FALSE) 

cm_d_p
