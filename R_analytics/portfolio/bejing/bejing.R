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

df <- read.csv(file.choose(), header = TRUE) #bejing data

glimpse(df)

colnames(df) <- c("No",'Year',"Month","Day","Hour","PM25","dwep",'temp','pres','cbwd',
                  'lws','lr','ls')


df.backup <- df

#drop No column
df$No <- NULL

#remove NAs
df <- na.omit(df)

#get target variable
Target <- df$PM25
df$PM25 <- NULL

#categorical variables
df_cat <- df[,c(8)]
#df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")
df <- df[,-c(8)]

#CTS variables
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df[,c(5,6,8,9,10)] <- as.data.frame(lapply(df[,c(5,6,8,9,10)], normalize))

df <- cbind(df,df_cat_new,Target)

#split into train and test
set.seed(123)
sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

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
R22=summary(fit)$r.squared