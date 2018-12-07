#remove old data
rm(list=ls())

#packages
packages = c('tidyverse','caret','mlbench','data.table', 'corplot', 'ggplot2', 'caTools')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
#load data
df <- read.csv(file.choose())

#get summary
summary(df)

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data

#check correlation
corrplot(cor(df[,1:15]), method = "number")

#set seed
set.seed(123)

#split data into train and test
sample <- sample.split(df,SplitRatio = 0.75)
training <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

#option 1 - use all variables
fit <- lm(Target ~., data=training)
summary(fit)
plot(Target ~., data=training)
plot(residuals(fit) ~ ., data=training)
#get accuracy and MSE
pred1 <- predict(fit, newdata = test)
mae <- function(error)
{
  mean(abs(error))
}
error <- pred1 - test$Target
MAE <- mae(error)
R2=summary(fit)$r.squared
#confusionMatrix(pred1,test$Target)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#option 2 - use normalized data + all variables
#normalize data
df_update <- normalize(df[,1:15])
df_update <- cbind(df_update, df$Target)

#split data into train and test
sample <- sample.split(df,SplitRatio = 0.75)
training <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

fit <- lm(df$Target ~., data=training)
summary(fit)
AIC(fit)
plot(Target ~., data=training)
#plot(residuals(fit) ~ ., data=training)
#get accuracy and MSE
pred1 <- predict(fit, newdata = test)

error <- pred1 - test$Target
MAE <- mae(error)
R2=summary(fit)$r.squared

#option 3 - normalize + feature importance
#normalize data
df_update <- normalize(df[,1:15])
df_update <- cbind(df_update, df$Target)

#find important features
# # calculate correlation matrix
correlationMatrix <- cor(df_update[,1:15])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
print(highlyCorrelated) #nothing highly correlated
#no sepcial correlation

#option - normalize + log
df_update <- normalize(df[,1:15])
df_update <- cbind(df_update, df$Target)

#split data
sample <- sample.split(df,SplitRatio = 0.75)
training <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

fit <- lm(log(df$Target) ~., data=training)
summary(fit)
AIC(fit)
#plot(Target ~., data=training)
pred1 <- predict(fit, newdata = test)

error <- pred1 - test$Target
MAE <- mae(error)
R2=summary(fit)$r.squared
#-----------------------------------------
fit <- lm(df$Target ~., data=log(training))
summary(fit)
AIC(fit)
pred1 <- predict(fit, newdata = test)

error <- pred1 - test$Target
MAE <- mae(error)
R2=summary(fit)$r.squared
#-------------------------------------------

fit <- lm(log(df$Target) ~., data=log(training))
summary(fit)
AIC(fit)
pred1 <- predict(fit, newdata = test)

error <- pred1 - test$Target
MAE <- mae(error)
R2=summary(fit)$r.squared


