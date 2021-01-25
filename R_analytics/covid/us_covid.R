#===================
# Objective
#===================
#Data source
#https://healthdata.gov/dataset/united-states-covid-19-cases-and-deaths-state-over-time
#/resource/7f8d9b63-9359-49d2-92bb#{view-graph:{graphOptions:
#{hooks:{processOffset:{},bindEvents:{}}}},graphOptions:{hooks:{processOffset:{},bindEvents:{}}}}

# - week over week change for cases and deaths for all states
# - Show a top 10 states have shown the greatest increase of cases and deaths in a given week

# - month over month change for cases and deaths for all states
# - Show a top 10 states have shown the greatest increase of cases and deaths in a given month

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

#===================
# load data
#===================
df <- read.csv("US_COVID.csv")

#select key columns
df_new <- df %>%
  select(submission_date, state, tot_cases, tot_death, consent_cases, consent_deaths)

# Update dates
df_new$submission_date <- lubridate::mdy(df_new$submission_date)
# Generate week
df_new$week <- lubridate::week(df_new$submission_date)
# Genrate month
df_new$month <- lubridate::month(df_new$submission_date)

# sort data
df_new <- df_new %>%
  arrange(state,submission_date)

# all states
state <- sort(unique(df_new$state))

#===================
# Week analysis
#===================
# cases
df_week_cases <- df_new %>%
  filter(consent_cases == "Agree") %>%
  group_by(state,week) %>%
  summarise(total_cases = sum(tot_cases)) %>%
  arrange(state,week) %>%
  select(state, week, total_cases)



# death
df_week_deaths <- df_new %>%
  filter(consent_deaths == "Agree") %>%
  group_by(state,week) %>%
  summarise(total_deaths = sum(tot_death)) %>%
  arrange(state,week) %>%
  select(state, week, total_deaths)


#===================
# month analysis
#===================
# cases
df_month_cases <- df_new %>%
  filter(consent_cases == "Agree") %>%
  group_by(state,month) %>%
  summarise(total_cases = sum(tot_cases)) %>%
  arrange(state,month) %>%
  select(state, month, total_cases)

# death
df_month_deaths <- df_new %>%
  filter(consent_deaths == "Agree") %>%
  group_by(state,month) %>%
  summarise(total_deaths = sum(tot_death)) %>%
  arrange(state,month) %>%
  select(state, month, total_deaths)

for (i in 1:(length(df$A)) {
  if (df$A[i] >= 15 && df$A[i+3] <= 12) {
    df$B[i] <- 1
  } else if ((df$A[i] <= 0) && (df$A[i+3] >= 10)) {
    df$B[i] <- 2
  }
  else {
    df$B[i] <- 0
  }
}

get_difference <- function (df){
  temp_data <- c()
  for (i in 1:length(df$state)){
    
    if (df$week == 1){
      temp_data[[i]] <- 0
    } else if (df$week[i]  ){
      
    } else {
      
    }
    
  }
  
  return (temp_data)
}





