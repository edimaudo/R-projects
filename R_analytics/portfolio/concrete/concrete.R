#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','readxl')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_excel(file.choose(), sheet = "Sheet1") #concrete data

glimpse(df)

summary(df)

#rename columns
colnames(df) <- c('cement','slag','ash','water','superplast','course_agg','fine_agg','age',"comp_strength")

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#scale data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- df[,1:8]
df_cts <- as.data.frame(lapply(df_cts, normalize))

#combine data
strength <- as.data.frame(df[,9])
df_new <- cbind(df_cts, strength)

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
fit <- lm(comp_strength ~., data=train)
summary(fit)
#get accuracy and MSE
pred1 <- predict(fit, newdata = test)

error <- pred1 - test$comp_strength
MAE <- mae(error)
R2=summary(fit)$r.squared

#check rfe
#rfe
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train[,1:length(df_new)], train[,9], sizes=c(1:length(df_new)), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#get accuracy and MSE
pred1 <- predict(results, newdata = test)
error <- pred1 - test$comp_strength
MAE2 <- mae(error)
R22=summary(fit)$r.squared
