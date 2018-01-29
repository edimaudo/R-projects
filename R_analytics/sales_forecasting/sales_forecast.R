#remove all data
rm(list=ls())

#libraries
library(forecast)
library(ggplot2)

sales_data <- read_csv(file.choose())

sales_data$Date <- as.Date(sales_data$Date, "%m/%d/%Y")

#split data
train_data <- sales_data[1:249,]
test_data <- sales_data[250:365, ]

#forecasting

