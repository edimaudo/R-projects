#remove all data
rm(list=ls())

#libraries
library(readxl)
library(tidyverse)
library(caret)
library(data.table)

#read data
airQ = as.data.frame(read_excel(file.choose())) #excel file

#backup
airQ.orig <- airQ

#sort by wind
airQ <- airQ %>%
  arrange(Wind)

#remove where ozone is NA
airQ <- airQ %>%
  filter(Ozone != "NA")

#Remove the Solar.R column and create a new column containing the ratio of Ozone to Wind.
airQ <- airQ %>%
  select (-Solar.R) %>%
  mutate(Ozone = as.numeric(Ozone)) %>%
  mutate(ozone_wind_ratio = Ozone / Wind) 
  
library(ggplot2)

  