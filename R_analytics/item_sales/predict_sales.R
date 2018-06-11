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
  "LoanStats_2016Q1.csv",
  "LoanStats_2016Q2.csv",
  "LoanStats_2016Q3.csv",
  "LoanStats_2016Q4.csv",
  "LoanStats_2017Q1.csv",
  "LoanStats_2017Q2.csv",
  "LoanStats_2017Q3.csv",
  "LoanStats3a.csv",
  "LoanStats3b.csv",
  "LoanStats3c.csv",
  "LoanStats3d.csv"
)

#clean filenames
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
#load the files
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

#data backup

#examine data

#find missing data



#data exploration

#data cleaning

#perform forecasting

#

