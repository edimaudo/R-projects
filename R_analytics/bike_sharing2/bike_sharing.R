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

#bike data
df_day <- read.csv(file.choose(), header = TRUE) 
df_hour <- read.csv(file.choose(), header = TRUE) 

glimpse(df_day)
glimpse(df_hour)

#goal predict daily data

df_day <- na.omit(df_day)

#drop instant column
df_day$instant <- NULL

#convert dteday to date
df_day$dteday <- as.Date(dteday)


#cts variables
df_cat <- df_day[,c(2:7)]
df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")
df_cat_new <- df_cat_new[,-c(4,6,10,20,27,29)]

#normalize 
df_cts <- df_day[,c(9:14)]
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- as.data.frame(lapply(df_cts, normalize))

#target variable
Target <- df_day$cnt

#aggregate data
df_new <- cbind(df_cat_new, df_cts, Target)

#remove highly correlated columns
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df_new)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

mae <- function(error)
{
  mean(abs(error))
}

#create train and test data
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#predict day
#rfe
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train[,1:ncol(train)-1], train[,ncol(train)], sizes=c(1:ncol(train)-1), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#get accuracy and MSE
pred1 <- predict(results, newdata = test)
error <- pred1 - test$Target
MAE2 <- mae(error)



