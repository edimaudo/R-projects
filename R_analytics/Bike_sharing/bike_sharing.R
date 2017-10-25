#libraries
library(psy)
library(nFactors)
library(MASS)
library(psych)
library(cluster)
library(factoextra)
library(corrplot)
library(lattice)
library(readxl)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ggpubr)
library(polycor) 

library(ggplot2)
library(forecast)
library(tseries)

#arima modeling

#arima cheat sheet
# Examine your data
# Plot the data and examine its patterns and irregularities
# Clean up any outliers or missing values if needed
# tsclean() is a convenient method for outlier removal and inputing missing values
# Take a logarithm of a series to help stabilize a strong growth trend
# Decompose your data
# Does the series appear to have trends or seasonality?
# Use decompose() or stl() to examine and possibly remove components of the series
# Stationarity
# Is the series stationary?
# Use adf.test(), ACF, PACF plots to determine order of differencing needed
# Autocorrelations and choosing model order
# Choose order of the ARIMA by examining ACF and PACF plots
# Fit an ARIMA model
# Evaluate and iterate
# Check residuals, which should haven no patterns and be normally distributed
# If there are visible patterns or bias, plot ACF/PACF. Are any additional 
#order parameters needed?
# Refit model if needed. Compare model errors and fit criteria such as AIC or BIC.

#load data
#daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)
daily_data = read.csv(file.choose(), sep = ',', header=TRUE, stringsAsFactors=FALSE)

daily_data.orig = daily_data

#check for outliers, volatility and irregularities
daily_data$Date = as.Date(daily_data$dteday)

ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  
+ ylab("Daily Bike Checkouts") +
  xlab("")

# moving average
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)


ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average")) 
+
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  
+
  ylab('Bicycle Count')


#calculate seasonal component
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

#dickey fuller test
adf.test(count_ma, alternative = "stationary")

Acf(count_ma, main='')

Pacf(count_ma, main='')

#augmented DF test
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#arima model
auto.arima(deseasonal_cnt, seasonal=FALSE)

#model validation
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

#
fit2 = arima(deseasonal_cnt, order=c(1,1,7))
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

#forecast horizon
fcast <- forecast(fit2, h=30)
plot(fcast)

#model performance
hold <- window(ts(deseasonal_cnt), start=700)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

#model performance with seasonality
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)