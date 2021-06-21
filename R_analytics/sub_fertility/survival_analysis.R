# Survivial analysis
# Survival analysis for a cohort of infertile couples attempting to conceive with censoring 
# for conception or withdrawal from study
#  1. Kaplan Meier survival analysis (plotted cumulatively or 1-x) with censoring for 2 events 
#  2. Cumulative competing risks method 
#  3.  graph(s) to explain the variables BMI, time trying to conceive and diagnosis type 3
# 

rm(list = ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'caret','dummies','mlbench','tidyr','Matrix','lubridate','survminer',
              'survival','data.table','vtreat', 'rsample','scales','readxl')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_excel(file.choose())

#backup data
df.backup <- df

#summary statistics
summary(df)

#  1. Kaplan Meier survival analysis (plotted cumulatively or 1-x) with censoring for 2 events 

#  2. Cumulative competing risks method 


#  3.  graph(s) to explain the variables BMI, time trying to conceive and diagnosis type 3