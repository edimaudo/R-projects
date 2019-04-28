#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools',
              'dummies','data.table')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.table(file.choose(),sep=",",header=FALSE) 

#update column names
colnames(df) <- c('var','skew','curtosis','entropy','class')

#get summary
summary(df)

#backup
df.backup <- df

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

ggplot(df,aes(x=as.factor(class))) + geom_bar() #split is close enough

#class
Class <- as.factor(df$class)


df_cts <- df[,c(1,4)]
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- as.data.frame(lapply(df_cts, normalize))

#combine data
df_new <- cbind(df_cts, Class)

#remove unnecessary columns
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df_new)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.35)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#split data
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#cross fold validation due to unbalanced data
control <- trainControl(method="repeatedcv", number=10, repeats=1, sampling = "smote")

#random forest
fit.rf <- train(Class~., data=train, method="rf",preProcess = c("scale", "center"), trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(Class~., data=train, method="gbm", trControl=control)
#svm
fit.svm <- train(Class~., data=train, method="svmRadial", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(randomforest = fit.rf, gradboost = fit.gbm, svm = fit.svm))

summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

#test results
test_scores <- predict(fit.rf, test)
confusionMatrix(test_scores, test$Class)