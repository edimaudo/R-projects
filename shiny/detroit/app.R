#================================================================================
# Shiny web app for Detroit Open Data Challenge 2024
# The goal is to create data visualization using the Open Data Portal data
#================================================================================
rm(list = ls())
################
#packages 
################
packages <- c(
  'ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
  'DT','Matrix','lubridate','data.table','plotly'
)
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

################
#Load data
################
youth<- read.csv("Youth_Risk_Behavioral_Surveillance_System_2017.csv")
gonorrhea<- read.csv("Number_of_Gonorrhea_Cases_by_Age_Group.csv")
chlamydia<- read.csv("Number_of_Chlamydia_Cases_by_Age_Group.csv")


################
#Data Setup
################