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