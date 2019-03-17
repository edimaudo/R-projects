#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#bike data
df_day <- read.csv(file.choose(), header = TRUE) 
df_hour <- read.csv(file.choose(), header = TRUE) 

glimpse(df_day)
glimpse(df_hour)

#goal predict daily data

df_day <- na.omit(df_day)

#drop instant column
df_day$instant <- NULL

#convert dteday to date
df_day$dteday <- as.Date(dteday)


#cts variables
df_cat <- df_day[,c(2:7)]
df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")

#normalize 
df_cts <- df_day[,c(9:14)]
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#target variable
Target <- df_day$cnt
#predict day

#predict hour