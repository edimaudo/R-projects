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

df <- read.csv(file.choose(), header = TRUE)

glimpse(df)

#backup data
df.backup <- df

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#descriptive stats
summary(df)

#density plot of 