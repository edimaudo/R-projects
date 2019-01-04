#clear old data
rm(list=ls())

#packages
packages <- c('tidyverse','ggplot2','corrplot','mlbench','caret','e1071','pROC','stringr','lubridate','tree','AUC')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data

train <- read.csv(file.choose())
test <- read.csv(file.choose())
y_train <- as.factor(read.csv(file.choose(), header = FALSE)[,1])

#inital approach
tree_classifier <- tree(y ~ ., data = cbind(train, y = y_train))
training_scores <- predict(tree_classifier, train)
roc_curve <- roc(predictions = training_scores[,2], labels = y_train)
auc(roc_curve)
plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)
test_scores <- predict(tree_classifier, X_test)

train <- cbind(train, y_train)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(7)

#logistic regression
fit.glm <- train(y_train~., data=train, method="glm", trControl=control)
#decision trees
fit.cart <- train(y_train~., data=train, method="rpart", trControl=control)
#LDA
fit.lda <- train(y_train~., data=train, method="lda", trControl=control)
#svm
fit.svm <- train(y_train~., data=train, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(y_train~., data=train, method="rf", trControl=control)
#bagged cart
fit.treebag <- train(y_train~., data=train, method="treebag", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(y_train~., data=train, method="gbm", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(logistic = fit.glm, cart = fit.cart, lda = fit.lda, 
                          svm = fit.svm, randomforest = fit.rf, 
                          baggedcart = fit.treebag, gradboost = fit.gbm))
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)
