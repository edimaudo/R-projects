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

train <- read.table(file.choose(),sep = ",", header = TRUE) #occupancy
test <- read.table(file.choose(),sep = ",", header = TRUE)

glimpse(train)

summary(train)

train$hours <- as.integer(format(as.POSIXct(strptime(train$date,"%d/%m/%Y %H:%M",tz="")) ,format = "%H"))
test$hours <- as.integer(format(as.POSIXct(strptime(test$date,"%d/%m/%Y %H:%M",tz="")) ,format = "%H"))
train$minutes <- as.integer(format(as.POSIXct(strptime(train$date,"%d/%m/%Y %H:%M",tz="")) ,format = "%M"))
test$minutes <- as.integer(format(as.POSIXct(strptime(test$date,"%d/%m/%Y %H:%M",tz="")) ,format = "%M"))

occupancy <- as.factor(train$Occupancy)
train_cts <- train[,c(2,3,4,5,6,8,9)]


#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

train_cts <- as.data.frame(lapply(train_cts, normalize))
train_new <- cbind(train_cts, train_occupancy)

#fine tune model
# # calculate correlation matrix
correlationMatrix <- cor(train_new[,1:7])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)


train_new <- train_new[,-c(highlyCorrelated)]


#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(occupancy~., data=train, method="gbm", trControl=control)
  
results <- resamples(list(gradboost = fit.gbm))
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

#use test data
test_cts <- test[,c(2,3,4,5,6,8,9)]
test_cts <- as.data.frame(lapply(test_cts, normalize))
test_new <- test_new[,-c(highlyCorrelated)]
occupancy <- as.factor(test$Occupancy)
test_new <- cbind(test_cts, test_occupancy)
test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$room)