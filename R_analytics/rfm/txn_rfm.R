#clear old data
rm(list=ls())

#load libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)

#load excel file
df <- as.data.frame(read_excel(file.choose()))

#view data
glimpse(df)

#find missing
apply(df, 2, function(x) any(is.na(x)))

#do rfm model
rfm_data <- df %>%
  select(CUSTNO,TRANDATE,SALESTXN) %>%
  drop_na()

#convert date
rfm_data <- rfm_data %>%
  mutate(TRANDATE = as.Date(TRANDATE,"%Y-%m-%d"))


rfm_data2 <- rfm_data %>%
  group_by(CUSTNO) %>%
  summarise(recency=(as.numeric(as.Date(today())-max(TRANDATE))),
            frequency=n(), monetary= sum(SALESTXN))

