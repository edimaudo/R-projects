#libraries
library(caret)
library(corrplot)
library(ggplot2)
library(pROC)
library(LogicReg)
library(randomForest)
library(plyr)

#clear old data
rm(list=ls())

#use filechooser to select csv file
mydata <- read.table(file.choose(), sep = ',')

#delete 1 rows
mydata <- mydata[-c(1), ]

columninfo <- c('Recency (months)', 
                'Frequency (times)', 'Monetary (c.c. blood)', 
                'Time (months)', 'whether he/she donated blood in March 2007')

#rename column info
names(mydata) <- columninfo

#View
View(mydata)

mydata.orig = mydata #save orig data copy

mydata <- na.omit(mydata) # listwise deletion of missing

#summary
summary(mydata)

#correlation
corinfo <- mydata[,1:4]
corrplot(cor(corinfo), method="number")

#Prediction 
set.seed(1234)
splitIndex <- createDataPartition(data$class, p = .80,list = FALSE, times = 1)
trainSplit <- data[ splitIndex,]
testSplit <- data[-splitIndex,]
print(table(trainSplit$class))

#logistic regression
ctrl <- trainControl(method = "cv", number = 5)
modelglm <- train(as.factor('whether he/she donated blood in March 2007') ~. , data = trainSplit, method = "glm", trControl = ctrl)
summary(modelglm)
### predict
predictors <- names(trainSplit)[names(trainSplit) != 'whether he/she donated blood in March 2007']
predglm <- predict(modelglm, testSplit)
summary(predglm)
### score prediction using AUC
confusionMatrix(predglm, testSplit$class)

aucglm <- roc(as.numeric(testSplit$'whether he/she donated blood in March 2007'), as.numeric(predglm),  ci=TRUE)
plot(aucglm, ylim=c(0,1), print.thres=TRUE, 
     main=paste('Logistic Regression AUC:',round(aucglm$auc[[1]],3)),col = 'blue')