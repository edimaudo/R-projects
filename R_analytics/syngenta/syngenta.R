#remove old data
rm(list=ls())

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse','caret','mlbench')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
performance <- read_csv(file.choose())

glimpse(performance)

#check for missing data
missing_data <- apply(performance, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

# visualization of performance data
