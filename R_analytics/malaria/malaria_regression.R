#remove all data
rm(list=ls())

#libraries
library(readxl)
library(tidyverse)
library(caret)
library(mlbench)
library(data.table)

#read data
df = as.data.frame(read_excel(file.choose())) #excel file

