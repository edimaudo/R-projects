
# Oxford 
# Dataset contains two primary tables:
# Visits
# Date
# User meta data: postal code, gender, age-range
# Unordered list of stores visited
# Name of store, length of time stayed
# Store
# Metadata about each store
# Distance to each other store; distance to entrance; etc.
# Your tasks: analyze the data to find interesting patterns that Oxford Properties might be interested in
# Which stores are visited together most/least frequently?
# Do they change by time of day, day of week, time of year?
# Which stores are most frequently visited together that aren't that close together?
#analysis - association rules + recommendation


#remove old data
rm(list=ls())

#load libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)
library(colorspace)
library(arules)
library(arulesViz)

#set working direction
setwd("/Users/edima/Documents/Queens/courses/big_data_865/project/oxford/")

#load data
file_names = c(
  "O-D Distance Matrix.csv",
  "Unit No.csv",
  "Yorkdale August top stores visited.csv",
  "Yorkdale September top stores visited.csv",
  "Yorkdale October top stores visited.csv",
  "Yorkdale November top stores visited.csv",
  "Yorkdale December top stores visited.csv"
)

#clean filenames
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
#load the files
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

#exploratory analysis

#
