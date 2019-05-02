#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','readxl','dummies')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_excel(file.choose()) #data

glimpse(df)

summary(df)

heat <- as.data.frame(df[9])
cold <- as.data.frame(df[10])



#one hot encode X6,X8
df_cat <- df[,c(6,8)]
df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")
#drop columns
df_cat_new[10] <- NULL
df_cat_new[4] <- NULL

#scale data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- df[,c(1,2,3,4,5,7)]
df_cts <- lapply(df_cts, function(x) as.double(x))
df_cts <- as.data.frame(lapply(df_cts, normalize))

#combine data
df_new <- cbind(df_cat_new,df_cts, heat)

#==================
#predict heating
#==================

#fine tune model
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df_new)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#split data
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

mae <- function(error)
{
  mean(abs(error))
}

#models lm
fit <- lm(Y1 ~., data=train)
summary(fit)
#get accuracy and MSE
pred1 <- predict(fit, newdata = test)

error <- pred1 - test$Y1
MAE <- mae(error)
R2=summary(fit)$r.squared

#rfe
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train[,1:length(df_new)], train[,length(df_new)], sizes=c(1:length(df_new)), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#get accuracy and MAE
pred1 <- predict(results, newdata = test)
error <- pred1 - test$Y1
MAE2 <- mae(error)

#==================
#predict cooling
#==================
#one hot encode X6,X8
df_cat <- df[,c(6,8)]
df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")
#drop columns
df_cat_new[10] <- NULL
df_cat_new[4] <- NULL

#scale data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- df[,c(1,2,3,4,5,7)]
df_cts <- lapply(df_cts, function(x) as.double(x))
df_cts <- as.data.frame(lapply(df_cts, normalize))

#combine data
df_new <- cbind(df_cat_new,df_cts, cold)

#fine tune model
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df_new)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#split data
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)


fit <- lm(Y2 ~., data=train)
summary(fit)
#get accuracy and MSE
pred1 <- predict(fit, newdata = test)

error <- pred1 - test$Y2
MAE <- mae(error)
R2=summary(fit)$r.squared

#rfe
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train[,1:length(df_new)], train[,length(df_new)], sizes=c(1:length(df_new)), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#get accuracy and MAE
pred1 <- predict(results, newdata = test)
error <- pred1 - test$Y2
MAE2 <- mae(error)