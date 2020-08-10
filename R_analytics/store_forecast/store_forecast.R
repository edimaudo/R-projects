

##Objective
#Create a timeseries forecasting model for daily sales prediction.
#The model should incorporate multiple seasonalities 
#and lags for the promotional periods. 
#There are over 1100 different stores, with 4 categories. 
#The model(s) should be able to make forecast based on the store and type of store* - maybe look at categotity 
#validate the model trend, multiple seasonalities, and lags 

rm(list = ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'SnowballC','wordcloud','dplyr','tidytext','readxl','scales','forecast','prophet')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


#load data
store <- read.csv("store.csv")
train <- read.csv("train.csv")

glimpse(store)
glimpse(train)

#combine data

store_train <- train %>%
  inner_join(store, by = "Store")

#build forecasting model

#https://rpubs.com/steffanigomez/forecastingtutorial
#https://rstudio-pubs-static.s3.amazonaws.com/300439_029f7ba592794d1cb2dac8f09d139f51.html
#https://github.com/rjshanahan/Time-Series-Forecasting-with-Shiny


#build shiny model app
