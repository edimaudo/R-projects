library(class)
library(dplyr)
library(lubridate)
set.seed(100)

#remove all data
rm(list=ls())

mydata <- read.csv(file.choose(),sep=",")

#summary
summary(mydata)

stocks$Date <- ymd(stocks$Date)
stocksTrain <- year(stocks$Date) < 2014

predictors <- cbind(lag(stocks$Apple, default = 210.73), 
                    lag(stocks$Google, default = 619.98), 
                    lag(stocks$MSFT, default = 30.48))

prediction <- knn(predictors[stocksTrain, ], predictors[!stocksTrain, ], stocks$Increase[stocksTrain], k = 1)
table(prediction, stocks$Increase[!stocksTrain])
accuracy <- rep(0, 10)
k <- 1:10
for(x in k){
  prediction <- knn(predictors[stocksTrain, ], predictors[!stocksTrain, ],
                    stocks$Increase[stocksTrain], k = x)
  accuracy[x] <- mean(prediction == stocks$Increase[!stocksTrain])
}

plot(k, accuracy, type = 'b')