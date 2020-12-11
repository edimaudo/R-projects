# =======================================================
# packages
# =======================================================
rm(list=ls())
packages <- c('ggplot2', 'corrplot','tidyverse','dplyr','tidyr',
              'caret','mlbench','caTools','scales','readxl',
              'doParallel','scales','catboost', 'Matrix','lubridate',
              'xts','TTR','forecast')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_excel(file.choose())


#This converts the dataframe into an xts object
df.xts <- xts(x = df$Percentage, order.by = df$Months) 

df.end <- floor(0.6*length(df.xts)) #select the first 60% of the data
df.train <- df.xts[1:df.end,] #assign the first 60% of the data to the train set
df.test <- df.xts[(df.end+1):length(df.xts),] #assign the most recent 40% to the test set

#Convert the xts object to the ts class.
df.start <- c(year(start(df.train)), month(start(df.train)))
df.end <- c(year(end(df.train)), month(end(df.train)))
df.train <- ts(as.numeric(df.train), start = df.start, 
                   end = df.end, frequency = 12)

df.start <- c(year(start(df.test)), month(start(df.test)))
df.end <- c(year(end(df.test)), month(end(df.test)))
df.test <- ts(as.numeric(df.test), start = df.start,
                  end = df.end, frequency = 12)

#df.train
#df.test
#This will be how far out we will need our training model to predict in order to compare with our observed values for evaluating the model accuracy.
forecast.horizon <- length(df.test) 

#Decompose the Time Series
df.train.components <- decompose(df.train)
plot(df.train.components)


df.train %>%
  forecast(h=forecast.horizon) %>% 
  plot()
lines(seaice.test, col = "red")

#Exponential Smoothing Forecast account for the trend and seasonal components
df.train.esforecast <- HoltWinters(df.train,
                                       beta=TRUE, 
                                       gamma=TRUE) %>% 
  forecast(h=forecast.horizon)
accuracy(df.train.esforecast,df.test)

