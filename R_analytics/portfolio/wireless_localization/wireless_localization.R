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

df <- read.table(file.choose()) #wireless dataset

glimpse(df)

summary(df)

#rename columns
names(df) <- c("wifi1", "wifi2", "wifi3","wifi4","wifi5","wifi6", "wifi7","room")

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#factor class column
df$room <- as.factor(df$room)

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df[,1:7] <- as.data.frame(lapply(df[,1:7], normalize))

resultdata <- function(control, train){
  set.seed(123)
  #decision trees
  fit.cart <- train(room~., data=train, method="rpart", trControl=control)
  #LDA
  fit.lda <- train(room~., data=train, method="lda", trControl=control)
  #svm
  fit.svm <- train(room~., data=train, method="svmRadial", trControl=control)
  #random forest
  fit.rf <- train(room~., data=train, method="rf", trControl=control)
  #bagged cart
  fit.treebag <- train(room~., data=train, method="treebag", trControl=control)
  #boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
  fit.gbm <- train(class~., data=train, method="gbm", trControl=control)
  
  #------------------
  #compare models
  #------------------
  results <- resamples(list(cart = fit.cart, lda = fit.lda,
                            svm = fit.svm, randomforest = fit.rf, 
                            baggedcart = fit.treebag, gradboost = fit.gbm))
  
  return (results)
}

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
fit.gbm <- train(room~., data=train, method="gbm", trControl=control)
test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$class)

