#===================
# Objective
#===================

#Data source
#https://healthdata.gov/dataset/united-states-covid-19-cases-and-deaths-state-over-time/resource/7f8d9b63-9359-49d2-92bb#{view-graph:{graphOptions:{hooks:{processOffset:{},bindEvents:{}}}},graphOptions:{hooks:{processOffset:{},bindEvents:{}}}}

# - week over week growth rates for cases and deaths for all states
# - month over month growth rates for cases and deaths for all states
# - Show a top 10 states have shown the greatest increase of cases and deaths in a given 
# - day
# - week
# - month

#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel', 'lubridate')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}



#load data
df <- read.csv("US_COVID.csv")

df_new <- df %>%
  select(submission_date, state, tot_cases, tot_death, consent_cases, consent_deaths)

#update dates
df_new$submission_date <- lubridate::mdy(df_new$submission_date)
#Generate week
df_new$week <- lubridate::week(df_new$submission_date)
#Geenrate month
df_new$month <- lubridate::month(df_new$submission_date)

#Week growth 

# month growth