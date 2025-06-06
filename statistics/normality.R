#clear old data
rm(list=ls())

library(readxl)         # read in Excel data
library(ggplot2)        # create visualizations
library(psych)          # provide summary statistics
library(pastecs)        # provide summary statistics
library(tidyverse)

golf <- read_excel(file.choose(), sheet = "2011") #golf data