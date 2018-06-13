#remove old data
rm(list=ls())

#load libraries
library(ggplot2)
library(corrplot)
library(tidyverse)
library(lubridate)
library(data.table)
library(prophet)
library(readxl)

#load excel file
df = as.data.frame(read_excel(file.choose()))

df.old <- df

#use only days and number of bicycles
df <- df[,1:2]
