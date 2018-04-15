#clear old data
rm(list=ls())

#load libraries
library(tidyverse)
library(ggplot2)
library(readxl)

#load excel file
df <- as.data.frame(read_excel(file.choose()))

#view data

#find missing

#do rfm model
