#clear enviroment
rm(list=ls())

#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
#load data
df <- read.table(file.choose(),sep=",")

glimpse(df)

#rename columns
names(df) <- c('buying', 'paint', 'doors', 'persons', 'lug_boot', 'safety','class')

#recode class
df$class <- recode_factor(df$class, "acc" = "2","good" = "3","vgood" = "4", "unacc" = "1")
#recode buying
df$buying <- recode_factor(df$buying, "low" = "1","med" = "2","high" = "3", "vhigh" = "4")
#recode paint
df$paint <- recode_factor(df$paint,  "low" = "1","med" = "2","high" = "3", "vhigh" = "4")
#recode lugboot
df$lug_boot <- recode_factor(df$lug_boot, "small" = "1","med" = "2","big" = "3")
#recode safety
df$safety <- recode_factor(df$safety, "low" = "1","med" = "2","high" = "3")
#recode person
df$persons <- recode_factor(df$persons, "2" = "1","4" = "2","more" = "3")

#------
#predict model
#------
#split data into train and test
sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

set.seed(123)
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#logistic regression
fit.glm <- train(class~., data=train, method="glm", trControl=control)
#decision trees
fit.cart <- train(class~., data=train, method="rpart", trControl=control)
#LDA
fit.lda <- train(class~., data=train, method="lda", trControl=control)
#svm
fit.svm <- train(class~., data=train, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(class~., data=train, method="rf", trControl=control)
#bagged cart
fit.treebag <- train(class~., data=train, method="treebag", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(class~., data=train, method="gbm", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(cart = fit.cart, 
                          svm = fit.svm, randomforest = fit.rf, 
                          baggedcart = fit.treebag, gradboost = fit.gbm))
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)



class <- df$class

df_categorical <- df %>%
  dplyr::select(buying, paint, doors, persons, lug_boot, safety)

#hot hot encode all columns
library(dummies)
df_cat_new <-  dummy.data.frame(df_categorical, sep = "_")

df_new <- cbind(df_cat_new, class)

#-----------
#new prediction
#-----------
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#logistic regression
fit.glm <- train(class~., data=train, method="glm", trControl=control)
#decision trees
fit.cart <- train(class~., data=train, method="rpart", trControl=control)
#LDA
fit.lda <- train(class~., data=train, method="lda", trControl=control)
#svm
fit.svm <- train(class~., data=train, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(class~., data=train, method="rf", trControl=control)
#bagged cart
fit.treebag <- train(class~., data=train, method="treebag", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(class~., data=train, method="gbm", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(cart = fit.cart, 
                          svm = fit.svm, randomforest = fit.rf, 
                          baggedcart = fit.treebag, gradboost = fit.gbm))
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

#use of test data
test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$class)