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
colnames(df_transactions) <- c("customer_name","offer_id")

glimpse(df_transactions)

df_combine <- df_transactions %>%
  inner_join(df_offers,"offer_id")