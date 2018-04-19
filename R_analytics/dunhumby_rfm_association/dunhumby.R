#clear old data
rm(list=ls())

#libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)

#load data
setwd("/Users/edima/Documents/Coding/R/R_analytics/dunhumby_rfm_association/") 

#clean filenames
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
#load the files
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

#glijmpse data
glimpse(hh_demographic)
glimpse(product)
glimpse(transaction_data0416)

#market basket
data_info <- product %>%
  inner_join(transaction_data0416,"PRODUCT_ID") %>%
  select(PRODUCT_ID, COMMODITY_DESC, household_key)

#items and transactions
mba_app <- as.data.frame(data_info)

library(arules)
#prep data for apriori algorithm
mba_app_trans <- as(split(mba_app[,"COMMODITY_DESC"], 
                          unique(mba_app[,"PRODUCT_ID"])), "transactions")

#rules
mba_app_rules <- apriori(mba_app_trans, 
                         parameter = list(supp = 0.06, conf = 0.75, 
                                          target = "rules", minlen = 2))
summary(mba_app_rules)