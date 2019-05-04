#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv(file.choose(), sep = ";")

glimpse(df)

summary(df)

df.backup <- df
df <- df.backup

#recode y
df[,21] <- ifelse(df[,21] == "no", "0", "1")
df[,21] <- as.factor(df[,21])
y = df[,21]

#recode categorical
df_cat <- df[,c(2,3,4,5,6,7,8,9,10,15)]
df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")

#normalize cts variables
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- df[,c(1,11,12,13,14,16,17,18,19,20)]
df_cts <- as.data.frame(lapply(df_cts, normalize))

#combine data
#combine data
df_new <- cbind(df_cat_new,df_cts, y)

#fine tune model
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df_new)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#split data
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#generate model
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)

#modelnames <- paste(names(getModelInfo()), collapse=',  ') #get model names
#adaboost
fit.adaboost <- train(y~., data=train, method="adaboost", trControl=control)
#adabag
fit.adabag <- train(y~., data=train, method="adaBag", trControl=control)
#random forest
fit.rf <- train(y~., data=train, method="rf", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(y~., data=train, method="gbm", trControl=control)
#svm
fit.svm <- train(y~., data=train, method="svmRadial", trControl=control)
#gbm h20
fit.gbmh2o <- train(y~., data=train, method="gbm_h2o", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(randomforest = fit.rf, gradboost = fit.gbm, svm = fit.svm))

summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$Target)

