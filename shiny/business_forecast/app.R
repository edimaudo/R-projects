#=============
# Business Forecast
#=============

rm(list = ls()) #clear environment
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse','readxl',
              'shiny','shinydashboard','scales','dplyr','mlbench','caTools',
              'forecast','TTR','xts','lubridate')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#=============
# Load data
#=============
forecast_df <- read_excel("Forecast Data.xlsx",sheet="Data")
year_info <- read_excel("Forecast Data.xlsx",sheet="YearInfo")
week_info <- read_excel("Forecast Data.xlsx",sheet="WeekInfo")

#=============
# Forecast Inputs
#=============
horizon_info <- c(1:50) #default 14
frequency_info <- c(7, 12, 52, 365)
difference_info <- c("Yes","No")
log_info <- c("Yes","No")
model_info <- c('auto-arima','auto-exponential','simple-exponential',
                'double-exponential','triple-exponential', 'tbat')

#=============
# Define UI for application
#=============