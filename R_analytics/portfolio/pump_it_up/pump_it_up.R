#remove old data
rm(list=ls())
#packages
packages <- c('tidyverse','caret','mlbench', 'caTools','mice','dummies')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

train <- read.csv(file.choose()) #pump
test <- read.csv(file.choose())

glimpse(train)

summary(train)

#check for missing data
missing_data_train <- apply(train, 2, function(x) any(is.na(x)))
missing_data_test <- apply(test, 2, function(x) any(is.na(x)))

print(missing_data_train) # no missing data
print(missing_data_test) #no missing data

