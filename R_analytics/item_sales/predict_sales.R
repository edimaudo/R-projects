#clear old data
rm(list=ls())

#load libraries
library(ggplot2)
library(corrplot)
library(tidyverse)
library(lubridate)
library(data.table)

#set working directory
setwd("/Users/edima/Documents/Coding/R/R_analytics/item_sales/") 

#load data
file_names = c(
  "item_categories.csv",
  "items.csv",
  "sales_train.csv",
  "shops.csv",
  "test.csv"
)

#clean filenames
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
#load the files
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

#data backup
item_categories.backup <- item_categories
item_categories.backup <- items
shops.backup <- shops

#find missing data - no missing data
missing_data_item_cat <- apply(item_categories, 2, function(x) any(is.na(x)))
missing_data_items <- apply(items, 2, function(x) any(is.na(x)))
missing_data_shops <- apply(shops, 2, function(x) any(is.na(x)))

#data cleaning
consolidated = sales_train %>%
  left_join(items, by=c("item_id"))

###Since we are not using name,will remove item name
consolidated$item_name=NULL

###is there any Missing obesrvation
colSums(is.na(consolidated))

consolidated$date=dmy(consolidated$date)
consolidated$month=month(consolidated$date)
consolidated$year=year(consolidated$date)
consolidated$day=weekdays(consolidated$date)

consolidated$day=as.factor(consolidated$day)
consolidated$month=as.factor(consolidated$month)
consolidated$year=as.factor(consolidated$year)

glimpse(consolidated)

#data exploration & visualization
##Stores by Sales
consolidated %>%
  group_by(shop_id) %>%
  summarise(total_sales = sum(item_cnt_day)) %>%
  arrange(desc(total_sales))%>%
  head(15)%>%
  ggplot(aes(x = reorder(as.factor(shop_id), total_sales), y = total_sales,fill=as.factor(shop_id))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+
  labs(y = 'Total unit sales', x = 'Stores', title = 'Total Sales by Store') +
  coord_flip()

##Most frequent items by store
consolidated %>%
  group_by(shop_id) %>%
  summarise(count = n_distinct(item_id)) %>%
  arrange(desc(count)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(as.factor(shop_id), count), y = count,fill=as.factor(shop_id))) +
  geom_bar(stat = 'identity') +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Stores", y = "Frequency of item",title="Most frequent items by store")

##Most frequent categories by store

consolidated %>%
  group_by(shop_id) %>%
  summarise(count = n_distinct(item_category_id)) %>%
  arrange(desc(count)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(as.factor(shop_id), count), y = count,fill=as.factor(shop_id))) +
  geom_bar(stat = 'identity') +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Stores ", y = "Frequency of Category",title="Most frequent categories by store")

#perform forecasting


