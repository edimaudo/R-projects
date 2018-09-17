#clear old data
rm(list=ls())

#libraries
library(readxl)
library(ggplot2)
library(tidyverse)

#read data
supermarket <- read_excel(file.choose(), sheet = 2) #supermarket data

