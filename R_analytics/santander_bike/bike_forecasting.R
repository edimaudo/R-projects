#remove old data
rm(list=ls())

#load libraries
library(ggplot2)
library(corrplot)
library(tidyverse)
library(lubridate)
library(data.table)
library(prophet)
library(readxl)

#load excel file
df = as.data.frame(read_excel(file.choose()))

df.old <- df

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data

#use only the first two columns
df <- df %>%
  rename(ds = Day, y = `Number of Bicycle Hires`) %>%
  select(ds,y)

#set seed for reproducability
set.seed(123)

#perform forecasting
m <- prophet(df)

future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)

prophet_plot_components(m, forecast)
