#remove old data
rm(list=ls())

#load libraries
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools', 
              'MASS','Metrics','randomForest','lars','xgboost','Matrix','methods', 'lubridate',
              'data.table')

for (package in packages) {
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


