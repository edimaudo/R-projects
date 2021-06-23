rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl',
              'scales','dplyr','mlbench','caTools','forecast','TTR','xts',
              'lubridate')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv("test-ts2.csv")
glimpse(df)

#update date field
df$Arrival_date <- lubridate::dmy(df$Arrival_date)

horizon_info <- 14
frequency_info <- 365.25

patient.xts <- xts(x = df$Patients, order.by = df$Arrival_date) 

df$Patients2 <- log(df$Patients)

#aggregations
patient.daily <- apply.daily(patient.xts,mean)
patient.weekly <- apply.weekly(patient.xts, mean) 
patient.monthly <- apply.monthly(patient.xts, mean) 

patient.end <- floor(0.8*length(patient.daily)) #select the first 80% of the data
patient.train <- patient.daily[1:patient.end,] 
patient.test <- patient.daily[(patient.end+1):length(patient.daily),]

patient.start <- c(year (start(patient.train)), month(start(patient.train)),day(start(patient.train)))
patient.end <- c(year(end(patient.train)), month(end(patient.train)), day(end(patient.train)))
patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                   end = patient.end, frequency = 7)

patient.start <- c(year (start(patient.test)), month(start(patient.test)),day(start(patient.test)))
patient.end <- c(year(end(patient.test)), month(end(patient.test)), day(end(patient.test)))
patient.test <- ts(as.numeric(patient.test), start = patient.start, 
                   end = patient.end, frequency = 7)

forecast.horizon <- length(patient.test) 




## twice-difference the CO2 data
patient.train <- diff(patient.train, differences = 1)
#Decompose the Time Series
patient.train.components <- decompose(patient.train)
plot(patient.train.components)


# acf(patient.train)
# acf(log(patient.train))
# pacf(patient.train)
# pacf(log(patient.train))
# 
# df1 <- df[,c(1,2)]
patient.end <- floor(1*length(patient.daily)) #select the first 80% of the data
patient.train <- patient.daily[1:patient.end,] 
patient.test <- patient.daily[(patient.end+1):length(patient.daily),]

patient.start <- c(year (start(patient.train)), month(start(patient.train)),day(start(patient.train)))
patient.end <- c(year(end(patient.train)), month(end(patient.train)), day(end(patient.train)))
patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                    end = patient.end, frequency = 7)

patient.train.components <- decompose(patient.train)
plot(patient.train.components)



#Exponential Smoothing Forecast account for the trend and seasonal components
patient.train.esforecast <- HoltWinters(patient.train,
                                       beta=TRUE, 
                                       gamma=TRUE) %>% 
forecast(h=forecast.horizon)


fit_ets <- ets(patient.train)
# Automated forecasting using an ARIMA model
fit_arima <- auto.arima(patient.train)

patient.train%>% 
  HoltWinters(beta = TRUE, gamma = TRUE) %>% 
  forecast(h=forecast.horizon) %>% 
  plot()
lines(patient.test, col = "red")

#Auto forecast
patient.train %>%
  forecast(h=forecast.horizon) %>% 
  plot()
lines(patient.test, col = "red")

#Model accuracy
accuracy(patient.train.esforecast,patient.test)

infoOutput <- data_frame(accuracy(patient.train.esforecast,patient.test))

data_frame()