#objective
#build a text classifier in ( R ) for safety observations to classify the (object) and (failure mode).

# remove old data
rm(list=ls())

#libraries
library(corrplot)
library(tidyverse)
library(lubridate)
library(wordcloud2)
library(data.table)
library(stringr)
library(mlbench)
library(caret)
library(xgboost)
library(tidytext)

#load safety data
df <- read.csv(file.choose(), stringsAsFactors = FALSE)

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data

#==============
#visualization
#==============

#visualize object

#visualize failure

#visualize object and failure

#clean text

#visualize text using word cloud

#==============
#build classifier
#==============
