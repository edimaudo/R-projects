#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies','readxl')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_excel(file.choose()) #credit data

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

x <- df[1,]

x2 <- as.data.frame(x)
#delete first row
df <- df[-c(1),]

#delete first column
df[1] <- NULL

colnamesinfo <- as.data.frame(colnames(x2[,3:24]))
colnames(colnamesinfo) <- c("columns")

Target <- as.factor(df$Y)

df_cat <- df[,c(2,3,4,6,7,8,9,10,11)]
#convert to factor
df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")

df_cts <- df[,c(5,12:23)]

#convert chr to float
df_cts <- lapply(df_cts, function(x) as.double(as.character(x)))

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- as.data.frame(lapply(df_cts, normalize))

df_new <- cbind(df_cat_new, df_cts, Target)

#remove redundant columns
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

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)


#random forest
fit.rf <- train(Target~., data=train, method="rf", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(Target~., data=train, method="gbm", trControl=control)
#svm
fit.svm <- train(Target~., data=train, method="svmRadial", trControl=control)


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