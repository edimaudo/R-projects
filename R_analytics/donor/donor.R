#remove old data
rm(list=ls())

#load libraries
library(caret)
library(mlbench)
library(caTools)
library(tidyverse)
library(dummies)

df <- read_csv(file.choose())

glimpse(df)

#data backup
df.backup <- df

#summary 
print(summary(df))

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#can't drop missing data - so update column
my_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df <- df %>% 
  mutate_if(is.numeric, 
            .funs = funs(
              ifelse(is.na(.), 
                     median(., na.rm = TRUE),
                     .))) %>%
  mutate_if(is.character, 
            .funs = funs(
              ifelse(is.na(.), 
                     my_mode(.),
                     .)))

#check 

#build prediction model


#build rfm model
