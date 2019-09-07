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

df <- read.csv(file.choose(), sep = ",") #wholesale

glimpse(df)

summary(df)

df.backup <- df


Target <- as.factor(df$Channel)

df_cat <- as.factor(df$Region)

df_cts <- df[,3:8]

#one hot encode categorical
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")

#normalize cts variables
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- as.data.frame(lapply(df_cts, normalize))

df_new <- cbind(df_cat_new, df_cts, Target)

#remove highly correlated columns
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df_new)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#create train and test data
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)

#modelnames <- paste(names(getModelInfo()), collapse=',  ') #get model names
#adaboost
#fit.adaboost <- train(Target~., data=train, method="adaboost", trControl=control)
#adabag
#fit.adabag <- train(Target~., data=train, method="adaBag", trControl=control)
#random forest
fit.rf <- train(Target~., data=train, method="rf", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(Target~., data=train, method="gbm", trControl=control)
#svm
fit.svm <- train(Target~., data=train, method="svmRadial", trControl=control)
#gbm h20
fit.gbmh2o <- train(Target~., data=train, method="gbm_h2o", trControl=control)

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