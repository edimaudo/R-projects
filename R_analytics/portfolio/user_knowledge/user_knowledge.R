#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','readxl')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
#load data
train <- read_excel(file.choose(), "Training_Data")
test <- read_excel(file.choose(), "Test_Data")

glimpse(train)

#summary statisitcs
summary(train)

