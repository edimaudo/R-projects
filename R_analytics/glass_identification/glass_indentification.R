#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','readxl')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.table(file.choose(),sep = ",") #glass identification

glimpse(df)

summary(df)

#rename columns
names(df) <- c("ID","RI","NA","MG","AL","SI","K","CA","BA","FE","Glass")

#drop the first column
df$ID <- NULL

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#recode Glass column
df$Glass <- as.factor(df$Glass)

resultdata <- function(control, train){
  set.seed(123)
  #LDA
  fit.lda <- train(Glass~., data=train, method="lda", trControl=control)
  #svm
  fit.svm <- train(Glass~., data=train, method="svmRadial", trControl=control)
  #random forest
  fit.rf <- train(Glass~., data=train, method="rf", trControl=control)
  #boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
  fit.gbm <- train(Glass~., data=train, method="gbm", trControl=control)
  
  #------------------
  #compare models
  #------------------
  results <- resamples(list(lda = fit.lda,svm = fit.svm, randomforest = fit.rf, gradboost = fit.gbm))
  
  return (results)
}

#normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df[,1:9] <-  as.data.frame(lapply(df[,1:9], normalize))


#split data into train and test
sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
results <- resultdata(control, train)

summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

#use test data
fit.gbm <- train(class~., data=train, method="gbm", trControl=control)
test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$Glass)