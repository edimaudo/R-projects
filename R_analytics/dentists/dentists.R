#===================
## Objective
#===================
#objective of this project is to conduct statistical analyses 
#to address a public health topic using appropriate statistical methods
 
#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel')

# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#===================
## Load Data
#===================
setwd("~/Documents/Coding/R/R_analytics/dentists")
df <- read_excel("Data Set Dentists BioStats-2.xlsx",sheet = "Data")

summary(df)

sapply(df, sd)