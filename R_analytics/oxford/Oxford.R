# Oxford 

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
setwd("/")

#load csv data
filenames = c(

)

#load the files
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

#exploratory analysis

#main analysis
