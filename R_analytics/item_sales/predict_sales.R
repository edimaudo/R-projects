#clear old data
rm(list=ls())

#load libraries
library(ggplot2)
library(corrplot)
library(tidyverse)
library(lubridate)
library(data.table)

#set working directory
setwd("/Users/edima/Documents/Coding/R/R_analytics/item_sales/") 

#load data
file_names = c(
  "item_categories.csv",
  "items.csv",
  "sales_train.csv",
  "shops.csv",
  "test.csv"
)

#clean filenames
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
#load the files
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

#data backup
item_categories.backup <- item_categories
item_categories.backup <- items
shops.backup <- shops

#find missing data - no missing data
missing_data_item_cat <- apply(item_categories, 2, function(x) any(is.na(x)))
missing_data_items <- apply(items, 2, function(x) any(is.na(x)))
missing_data_shops <- apply(shops, 2, function(x) any(is.na(x)))

#data exploration

#data cleaning

#perform forecasting


