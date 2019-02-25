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

train <- read.table(file.choose(),sep = ",", header = TRUE) #occupancy
test <- read.table(file.choose(),sep = ",", header = TRUE)

glimpse(train)

summary(train)

train$hours <- format(as.POSIXct(strptime(train$date,"%d/%m/%Y %H:%M",tz="")) ,format = "%H")
test$hours <- format(as.POSIXct(strptime(test$date,"%d/%m/%Y %H:%M",tz="")) ,format = "%H")
train$minutes <- format(as.POSIXct(strptime(train$date,"%d/%m/%Y %H:%M",tz="")) ,format = "%M")
test$minutes <- format(as.POSIXct(strptime(test$date,"%d/%m/%Y %H:%M",tz="")) ,format = "%M")

train_occupancy <- train$Occupancy
train_cts <- train[,2:6]
train_cat <- train[,8:9]


test_occupancy <- test$Occupancy
test_cts <- test[,2:6]
test_cat <- test[,8:9]

#normalize data
#normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

train_cts <- as.data.frame(lapply(train_cts, normalize))
test_cts <- as.data.frame(lapply(test_cts, normalize))

train_cat_new <- dummy.data.frame(as.data.frame(train_cat), sep = "_")
test_cat_new <- dummy.data.frame(as.data.frame(test_cat), sep = "_")

train_new <- cbind(train_cat_new , train_cts, train_occupancy)
test_new <- cbind(test_cat_new , test_cts, test_occupancy)

#fine tune model
# # calculate correlation matrix
correlationMatrix <- cor(train_new[,1:62])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#split data into train and test
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)