#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel')
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
crimes_data <- read.csv("uae_crime.csv",sep=";")
crimes_data <- crimes_data %>%
  mutate(Age = as.numeric(Age)) %>% # Convert age to numerical
  dplyr::select(-'Description.of.the.charge') %>% # Remove the description of the charge variable
  mutate_if(is.character, as.factor)# Convert character type variable to factor type

#review values
unique(crimes_data$Year)
unique(crimes_data$Gender)
unique(crimes_data$Age)
unique(crimes_data$`Marital Status`)
unique(crimes_data$`Educational Level`)
unique(crimes_data$Religion)
unique(crimes_data$Job)
unique(crimes_data$crime)

#people of age 0 where in the data. 
crimes_data <- crimes_data %>%
  filter(Age >= 15) %>%
  na.omit()

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
crime_info <- crimes_data_model %>%
  select(Year, Age)
crime_info2 <- crimes_data_model %>%
  select(Gender, Marital.Status, Education.Level, Religion,Job)
crime <- crimes_data_model %>%
  select(crime)
df_cts <- as.data.frame(lapply(crime_info, normalize))
df_cat <- as.data.frame(lapply(crime_info2, labelEncoder))
df_new <- cbind(df_cts,df_cat,crime)

set.seed(2020)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#model training
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs = TRUE)

#glm
fit.glm <- train(as.factor(crime)~., data=train, method="multinom", metric = "ROC", trControl = control)
#random forest
fit.rf <- train(as.factor(crime)~., data=train, method="rf", metric = "ROC", trControl = control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(as.factor(crime)~., data=train, method="gbm", metric = "ROC", trControl = control)
#svm
fit.svm <- train(as.factor(crime)~., data=train, method="svmRadial", metric = "ROC", trControl = control)
#nnet
fit.nnet <- train(as.factor(crime)~., data=train, method="nnet", metric = "ROC", trControl = control)
#naive
fit.naive <- train(as.factor(crime)~., data=train, method="naive_bayes", metric = "ROC", 
                   trControl = control)
#extreme gradient boosting
fit.xgb <- train(as.factor(crime)~., data=train, method="xgbTree", metric = "ROC", trControl = control)
#bagged cart
fit.bg <- train(as.factor(crime)~., data=train, method="treebag", metric = "ROC", trControl = control)
#decision tree
fit.dtree <- train(as.factor(crime)~., data=train, method="C5.0", metric = "ROC", trControl = control)
#knn
fit.knn <- train(as.factor(crime)~., data=train, method="kknn", metric = "ROC", trControl = control)


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

# Model accuracy
#mean(predicted.classes == test$crime)
#test data accuracy
# Make predictions
predicted.classes <- fit.xgb %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = test$crime, mode = "everything")




write.csv(as.data.frame(output$byClass),"output.csv")


#plot confusion matrix
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