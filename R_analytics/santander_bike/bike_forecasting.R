#remove old data
rm(list=ls())

#load libraries
library(ggplot2)
library(corrplot)
library(tidyverse)
library(lubridate)
library(data.table)

#load excel file
df = as.data.frame(read_excel(file.choose()))
