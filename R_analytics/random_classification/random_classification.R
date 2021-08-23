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
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# back up 
train.backup <- train 
test.backup <- test

glimpse(train)
glimpse(test)

#look for missing data
missing_data <- apply(train, 2, function(x) any(is.na(x)))
print(missing_data)

missing_data1 <- apply(test, 2, function(x) any(is.na(x)))
print(missing_data1)

# columns to drop
cols_remove <- c('x5','x11','x14','x16','x22','x24','x26','x30','x33','x38','x41','x42','x44','x45',
                 'x49','x52','x54','x55','x57','x61','x63','x64','x67','x68','x74','x75','x76','x77',
                 'x78','x79','x80','x83','x85','x86','x88','x89','x91','x92','x94','x95','x96','x99')


train <- train %>%
  select(-cols_remove)

test <- test %>%
  select(-cols_remove)

glimpse(train)


#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}

# update x3 if needed

# update x19 if needed

# update x31 if needed

# update x39 if needed

# update x60 if needed

# update x65 if needed

# update x93 if needed

#===================
# Calculate correlation
#===================
correlationMatrix <- cor(train)
corrplot(correlationMatrix,method='number')
# # summarize the correlation matrix
print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)