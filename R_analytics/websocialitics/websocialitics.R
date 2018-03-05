# remove old data
rm(list=ls())

#load data
mydata <- read.csv(file.choose())


#libraries
library(tidyverse)
library(ggplot2)

glimpse(mydata)

#Get a list of 10 product reviews that got review rating 5 and price more than thousand
product_review_list10 <- mydata %>%
  filter(ReviewRating == 5 & ProductPrice > 1000) %>%
  select(ReviewText) %>%
  top_n(n=10,ReviewText)

#How many products reviewed for every retailer?
product_review_retail <- mydata %>%
  group_by(RetailerName) %>%
  summarise(countinfo = n()) %>%
  select(RetailerName, countinfo)

#How many reviews for every product?
product_review_retail <- mydata %>%
  group_by(ProductModelName) %>%
  summarise(countinfo = n()) %>%
  select(ProductModelName, countinfo)

#Get a list of 5 reviews for shoppers in Chicago
chicago_shopper_review <- mydata %>%
  filter(RetailerCity == "Chicago") %>%
  select(ReviewText) %>%
  top_n(5,ReviewText)
  
#Find highest price product reviewed/sold in every city
highest_price_city <- mydata %>%
  group_by(RetailerCity) %>%
  summarise(
    highestPrice = max(ProductPrice)
  ) %>%
  select(RetailerCity,highestPrice)
  
#Get the total number of products reviewed and got Rating 5 in Every City
product_info_rating5_city <- mydata %>%
  filter(ReviewRating == 5) %>%
  group_by(RetailerCity) %>%
    summarise(reviewcount = n()) %>%
  select(RetailerCity, reviewcount)  
  
#What is the total number of reviews in every zipcode
review_zipcode <- mydata %>%
  group_by(RetailerZip) %>%
  summarise(reviewcount = n()) %>%
  select(RetailerZip, reviewcount)

library(dplyr)
library(data.table)
#bar chart show the total number of customers who complained 
#about heat issues for every product name of every manufacturer
check_heat <- mydata %>%
  filter(tolower(ReviewText) %like% "heat") %>%
  group_by(ManufacturerName,ProductModelName) %>%
  summarise(heatissuecount = n()) %>%
  select(ManufacturerName,ProductModelName, heatissuecount)

ggplot(data = check_heat, aes(x=ManufacturerName,y=heatissuecount,fill=ProductModelName)) + 
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()

#bar chart show the total number of customers who complained about Bluetooth issues for every 
#product name of every manufacturer from every retailer
check_bluetooth <- mydata %>%
  filter(tolower(ReviewText) %like% "bluetooth") %>%
  group_by(ProductModelName, ManufacturerName, RetailerName) %>%
  summarise(blueissuecount = n()) %>%
  select(ProductModelName, ManufacturerName, RetailerName, blueissuecount) 

ggplot(data = check_bluetooth, aes(x=reorder(RetailerName, -ManufacturerName) ,
                                   y=blueissuecount,fill=ProductModelName)) + 
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()

