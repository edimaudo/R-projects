#bill board analysis

#clear old data
rm(list=ls())

#load libraries
packages <- c('ggplot2', 'corrplot', 'data.table', 'lubridate',
              'stringr', "tidytext","tidyverse", "mlbench", "caret", "caTools")

#load libraries
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_csv(file.choose())

#get summary
print(summary(df))

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

glimpse(df$Song)