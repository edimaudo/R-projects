# MET Police Crime Forecast
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
# Data
#=============



#=============
# dropdowns
#=============
crime_info <- c()
aggregate_info <- c('weekly','monthly')
horizon_info <- c(1:50) 
frequency_info <- c(7, 12, 52, 365)
difference_info <- c("Yes","No")
log_info <- c("Yes","No")
model_info <- c('auto-arima','auto-exponential','simple-exponential',
                'double-exponential','triple-exponential', 'tbat')
                #other models ETS, ARIMA, STL-ETS, NNAR
                #https://otexts.com/fpp2/combinations.html