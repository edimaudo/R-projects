#remove old data
rm(list=ls())

#libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)

#load data using new system csv
df <- read.csv(file.choose(), sep= ",")

#glimpse of data
glimpse(df)

#no headers therefore give header names
names(df) <- c("start_time","end_time","begin_count","end_count","place")

#glimpse(df)

#look for missing data
apply(df, 2, function(x) any(is.na(x))) #no missing data

#core shopping hours are between 10am to 12am with some differences in weekends and holidays

#possible questions to ask
#Q1 - how to certain types of shops change over time (hours, day of week, quarterly)
#- break down shops by type
#-aggregate them and then visualize over time

#could use heatmap over time
#plot as timeseries -http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#6.%20Change
