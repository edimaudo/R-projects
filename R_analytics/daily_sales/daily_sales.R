# Objective
# In this competition you will work with a challenging time-series dataset 
# consisting of daily sales data, kindly provided by one of the largest Russian software firms - 1C Company. 
# 
# We are asking you to predict total sales for every product and store in the next month. 
# By solving this competition you will be able to apply and enhance your data science skills.
# 
# Data Description
# You are provided with daily historical sales data. 
# The task is to forecast the total amount of products sold in every shop for the test set. 
# Note that the list of shops and products slightly changes every month. 
#Creating a robust model that can handle such situations is part of the challenge.

# File descriptions
# sales_train.csv - the training set. Daily historical data from January 2013 to October 2015.
# test.csv - the test set. You need to forecast the sales for these shops and products for November 2015.
# sample_submission.csv - a sample submission file in the correct format.
# items.csv - supplemental information about the items/products.
# item_categories.csv  - supplemental information about the items categories.
# shops.csv- supplemental information about the shops.
# Data fields
# ID - an Id that represents a (Shop, Item) tuple within the test set
# shop_id - unique identifier of a shop
# item_id - unique identifier of a product
# item_category_id - unique identifier of item category
# item_cnt_day - number of products sold. You are predicting a monthly amount of this measure
# item_price - current price of an item
# date - date in format dd/mm/yyyy
# date_block_num - a consecutive month number, used for convenience. January 2013 is 0, 
# February 2013 is 1,..., October 2015 is 33
# item_name - name of item
# shop_name - name of shop
# item_category_name - name of item category

#remove old data
rm(list=ls())

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse','caret','mlbench', 'forecast', "fpp2","prophet")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


#load data
daily_sales <- read_csv(file.choose())
item_cateogories <- read_csv(file.choose())
items <- read_csv(file.choose())
shops <- read_csv(file.choose())
sales_train <- read_csv(file.choose())
test <- read_csv(file.choose())
sample_submission <- read_csv(file.choose())

#get summary
print(summary(sales_train))

#visualize data


#create model

#test data

#submit to kaggle