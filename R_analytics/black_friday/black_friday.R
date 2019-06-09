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

df <- read.csv(file.choose(), header = TRUE)

glimpse(df)

df.backup <- df

summary(df)

#plot gender
ggplot(df,aes(Gender)) + geom_bar()

#plot age and gender
ggplot(df, aes(x=Age,y=Gender)) + geom_bar(stat = "identity")

missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

df <- df[,c(3,4,5,6,7,8,9,12)]

Target <- df$Purchase

df_cat <- df[,c(1,2,4,5,6)]

df_cts <- df[,c(3,7)]

#one hot encoding
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- as.data.frame(lapply(df_cts, normalize))

#combine data
df_new <- cbind(df_cat_new, df_cts,Target)

#remove highly correlated columns
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df_new)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#create train and test data
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

mae <- function(error)
{
  mean(abs(error))
}

#models lm
fit <- lm(Target ~., data=train)
summary(fit)
#get accuracy and MSE
pred1 <- predict(fit, newdata = test)

error <- pred1 - test$Target
MAE <- mae(error)
R2=summary(fit)$r.squared

#rfe
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train[,1:length(train)-1], train[,length(train)], sizes=c(1:length(train)), rfeControl=control)
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
R22=summary(fit)$r.squared