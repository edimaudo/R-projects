#remove old data
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

#summary statisitcs
summary(df)

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#rename columns
names(df) <- c('industrial_risk', 'management_risk', 'financial_flexibility', 
               'credibility', 'competitiveness', 'operating_risk','class')

#recode columns
df$class <- recode_factor(df$class, "B" = "1","NB" = "2")
df$industrial_risk <- recode_factor(df$industrial_risk, "P"="3", "A"= "2", "N" = "1")
df$management_risk <- recode_factor(df$management_risk, "P"="3", "A"= "2", "N" = "1")
df$financial_flexibility <- recode_factor(df$financial_flexibility, "P"="3", "A"= "2", "N" = "1")
df$credibility <- recode_factor(df$competitiveness, "P"="3", "A"= "2", "N" = "1")
df$operating_risk <- recode_factor(df$operating_risk, "P"="3", "A"= "2", "N" = "1")



resultdata <- function(control, train){
  set.seed(123)
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
fit.gbm <- train(class~., data=train, method="gbm", trControl=control)
test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$class)
