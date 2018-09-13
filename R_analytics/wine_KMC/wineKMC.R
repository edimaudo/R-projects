#clear old data
rm(list = ls())

#load libraries
library(tidyverse)
library(readxl)

df<-NULL
df_offers <- read_excel(file.choose(),1)

colnames(df_offers) <- c("offer_id", "campaign", "varietal", "min_qty", "discount", "origin", "past_peak")
glimpse(df_offers)

df_transactions <- read_excel(file.choose(),2)