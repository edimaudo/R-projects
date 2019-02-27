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

df <- read.csv(file.choose()) #online news

glimpse(df)

summary(df)

##check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#columns to drop
#1st column
df[1] <- NULL

#fine tune model
# # calculate correlation matrix
correlationMatrix <- cor(df[,1:59])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)


df <- df[,-c(highlyCorrelated)]

#split into train and test
set.seed(123)
sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(shares~., data=train, method="gbm", trControl=control)


pred1 <- predict(fit.gbm, newdata = test)
mae <- function(error)
{
  mean(abs(error))
}
error <- pred1 - test$shares
MAE <- mae(error)
R2=summary(fit.gbm)$r.squared


