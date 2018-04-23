#remove all data
rm(list=ls())

#libraries
library(readxl)
library(tidyverse)
library(caret)
library(mlbench)
library(data.table)

#read data
df = as.data.frame(read_excel(file.choose())) #excel file

#view data
glimpse(df)

#check for missing data
apply(df, 2, function(x) any(is.na(x))) #no missing data

#set seed for reproducability
set.seed(123)

#split data into training and test
train<-sample_frac(df, 0.8)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-df[-sid,]

#check for correlations
library(corrplot)
corrplot(cor(train), method = "number")

#build initial model and test
linearMod <- lm(Malaria_Proportion ~., data=train)
summary(linearMod)
AIC(linearMod)

testPred <- predict(linearMod, test)
actuals_preds <- data.frame(cbind(actuals=test$Malaria_Proportion, predicteds=testPred))
correlation_accuracy <- cor(actuals_preds) 

#find important features
highlyCorrelated <- findCorrelation(cor(train), cutoff=0.5)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(Malaria_Proportion~., data=train, 
               method="lm", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


# load the data
data(train)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(df[,2:14], df[,1], sizes=c(1:14), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#build new model + check accuracy and AIC + use against test data
linearMod2 <- lm(Malaria_Proportion ~ City_Rivers + Elevation + City_Roads + 
                   Agriculture_fs_64K + Forest_fs_64K + Agriculture_fs_24K, data=train)
summary(linearMod2)
AIC(linearMod2)

testPred <- predict(linearMod2, test)
actuals_preds <- data.frame(cbind(actuals=test$Malaria_Proportion, predicteds=testPred))
correlation_accuracy <- cor(actuals_preds)
corrplot(correlation_accuracy, method = "number")