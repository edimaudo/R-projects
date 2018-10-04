# Context
# This dataset contains lot of historical sales data. 
# It was extracted from a Brazilian top retailer and has many SKUs and many stores. 
# The data was transformed to protect the identity of the retailer.

# Current inventory management models have many solutions to place the correct order,
# but they are all based in a single unknown factor: the demand for the next periods.
# This is why short-term forecasting is so important in retail and consumer goods industry.
# We encourage you to seek for the best demand forecasting model for the next 2-3 weeks. 
# This valuable insight can help many supply chain practitioners 
# to correctly manage their inventory levels.


#objective
#Short term forecasting to optimize in-store inventories

#remove old data
rm(list=ls())

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse','caret','mlbench', 'forecast', "fpp2","prophet")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_csv(file.choose())

#Rename columns
colnames(df) <- c('Date',"Sale","Stock","Price")

#create time series
df_other <- cbind(df[,1],df[,3])
colnames(df_other) <- c("ds","y")

#plot of information
autoplot(ts(df_other), facets = TRUE) +
  geom_smooth() +
  labs("Company Stocks",
       y = "Stock",
       x = NULL)

#use prophet
df_other_prophet <- prophet(df_other)
future <- make_future_dataframe(df_other_prophet, periods = 365)
forecast <- predict(df_other_prophet, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(df_other_prophet, forecast)
prophet_plot_components(df_other_prophet, forecast)

#multiplicative seasonality
m <- prophet(df, seasonality.mode = 'multiplicative')
forecast <- predict(m, future)
plot(m, forecast)

#component
prophet_plot_components(m, forecast)

#others 
m <- prophet(seasonality.mode = 'multiplicative')
m <- add_seasonality(m, 'quarterly', period = 91.25, fourier.order = 8, mode = 'additive')
m <- add_regressor(m, 'regressor', mode = 'additive')
