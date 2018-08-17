#remove all data
rm(list=ls())

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse','MASS', 'caret', 'mlbench')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- Boston 

#simple linear regression
fit <- lm(crim ~ medv, data=df)
fit <- lm(zn ~ medv, data=df)
fit <- lm(indus ~ medv, data=df)
fit <- lm(chas ~ medv, data=df)
fit <- lm(nox ~ medv, data=df)
fit <- lm(rm ~ medv, data=df)
fit <- lm(age ~ medv, data=df)
fit <- lm(dis ~ medv, data=df)
fit <- lm(rad ~ medv, data=df)
fit <- lm(tax ~ medv, data=df)
fit <- lm(ptratio ~ medv, data=df)
fit <- lm(black ~ medv, data=df)
fit <- lm(lstat ~ medv, data=df)

#multiple linear regression
output <- lm(medv~., data=df)
summary(output)
resid(output) #List of residuals
plot(density(resid(output))) #A density plot
qqnorm(resid(output))

#classification
#names(getModelInfo())

model_logreg<-train(df[,1:13],df[,14],method='logreg')
model_knn<-train(df[,1:13],df[,14],method='knn')
model_naive<-train(df[,1:13],df[,14],method='naive_bayes')

#Predictions
predictions<-predict.train(object=model_logreg,df,type="raw")
confusionMatrix(predictions,df[,14])

predictions_knn<-predict.train(object=model_knn,df,type="raw")
confusionMatrix(predictions_knn,df[,14])

predictions_naive<-predict.train(object=model_naive,df,type="raw")
confusionMatrix(predictions_naive,df[,14])