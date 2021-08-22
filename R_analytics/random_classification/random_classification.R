# Classify Y value

#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# Packages
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','readxl','gridExtra','grid')

# Load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


# Load data
train <- read.csv(file.choose())
test <- read.csv(file.choose())

glimpse(train)
glimpse(test)

train <- na.omit(train)
test <- na.omit(test)

#table(train$y)

summary(train)