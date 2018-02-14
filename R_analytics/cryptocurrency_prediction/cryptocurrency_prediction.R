#crypto
library(dplyr)
library(forecast)

#read in the data
mydata <- read.csv("/Users/edima/Documents/Coding/R/R_analytics/cryptocurrency_prediction/crypto-markets.csv")

symbolInfo <- unique(mydata$name)
symbolInfo1 <- symbolInfo %>%
  mutate(symbolInfo = as.character(symbolInfo)) %>%
  arrange(symbolInfo)

mydataTemp <- mydata %>%
  select(name, date, close)

mydataTemp <- mydataTemp %>%
  filter(name == "Bitcoin")

mydataTemp <- mydataTemp %>%
  select(date, close)

fit <- ts(mydataTemp)
plot(forecast(fit, ))