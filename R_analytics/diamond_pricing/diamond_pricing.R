#remove old data
rm(list=ls())

#load libraries
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools', 
              'MASS','Metrics','randomForest','lars','xgboost','Matrix','methods', 'readxl')
#load libraries
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_excel(file.choose()) #load the macro enabled file

#check data
glimpse(df)

#summary
print(summary(df))

#look for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

#check for missing data




#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}




