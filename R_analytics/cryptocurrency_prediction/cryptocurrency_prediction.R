#crypto
library(tidyverse)
library(forecast)

#read in the data
mydata <- read.csv("/Users/edima/Documents/Coding/R/R_analytics/cryptocurrency_prediction/crypto-markets.csv")

#newdata <- mydata[order(mydata$name),] 

#symbolInfo <- unique(newdata$name)

newdata1 <- mydata %>%
  filter(name == "0x") %>%
  select(date, close)
  


fit <- ts(newdata1, start=2013, end=2020, frequency = 12)
plot(forecast(fit[,2]), col="grey", main = "info", xlab="Times", ylab="Info")
lines(fit$time.series[,2],col="red",ylab="Trend")