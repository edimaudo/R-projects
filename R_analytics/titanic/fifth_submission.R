#libraries
library(caret)
library(corrplot)
library(ggplot2)
library(pROC)
library(LogicReg)
library(randomForest)



#load data
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

#view the data
head(train)

#remove empty data
train <- na.omit(train)

#use logistic regression
ctrl <- trainControl(method = "cv", number = 5)
modelglm <- train(train$Survived ~. , data = train, method = "glm", trControl = ctrl)
### predict

### score prediction using AUC

#AUC

#use random forests

#choose the best

#feature engineering

#logistic regression
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "fifthsubmission.csv", row.names = FALSE)