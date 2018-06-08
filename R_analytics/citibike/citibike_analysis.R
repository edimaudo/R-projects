#clear old data
rm(list=ls())

#load libraries
library(ggplot2)
library(corrplot)
library(tidyverse)

#load data
df <- read.csv(file.choose())

#View data
View(df)

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
View(missing_data)

