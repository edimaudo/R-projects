#clear old data
rm(list=ls())

#load libraries
library(ggplot2)
library(corrplot)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)
library(readxl)

#load data
df<- read_xlsx(file.choose())

#convert to json
library(jsonlite)

df_json <- jsonlite::toJSON(list(traits = names(df), values = df), pretty = TRUE)