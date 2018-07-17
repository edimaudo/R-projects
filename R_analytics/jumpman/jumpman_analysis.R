"""
Business background
Jumpman23 is an on-demand delivery platform connecting “Jumpmen” and customers purchasing a variety of goods. 
Jumpman23 will send Jumpmen to merchants to purchase and pickup any items requested by the customer. 
Whenever possible, Jumpman23 will order the requested items ahead to save the Jumpmen time. 
Each time a Jumpman23 delivery is completed, a record is saved to the Jumpman23 database that contains 
information about that delivery. 
Jumpman23 is growing fast and has just launched in its newest market -- New York City.

Objectives
Analyze the data with visualization

Outline data integrity issues + how it impacts the analysis

Data dictionary
● Job_ID -- > a unique identifier of a delivery
● Customer_id → a unique identifier for the Jumpman23 customer
● Jumpman_id → a unique identifier for the Jumpman who completed the delivery
● vehicle_type → The method of transport the Jumpman used to complete the delivery
● pickup_place → The name of the Pickup location
● place_category → A categorization of the Pickup location
● Item_name → the name of the item requested
● Item_quantity → how many of that item was requested
● Item_category_name → categorization provided by merchant, think “appetizers”,
“soups” etc
● How_long_it_took_to_order → how long it took to place the order [interval]
● pickup_lat → the coordinates of the pickup location
● pickup_lon → the coordinates of the pickup location
● dropoff_lat → the coordinations of the dropoff location
● dropoff_lon → the coordinations of the dropoff location
● when_the_delivery_started→ localized timestamp representing when the delivery
began
● when_the_Jumpman_arrived_at_pickup → localized timestamp representing when the
Jumpman arrived at the pickup location
● when_the_Jumpman_left_pickup → localized timestamp representing when the
Jumpman left the pickup location

● when_the_Jumpman_arrived_at_dropoff → localized timestamp representing when the Jumpman reached the customer
"""

#clear old data
rm(list=ls())

#load libraries
library(ggplot2)
library(corrplot)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)

#load data
df<- read.csv(file.choose())

#summary statistics
summary(df)

#data integrity issues
missing_data <- apply(df, 2, function(x) any(is.na(x)))
# item quantity has missing data

#empty(df)

#==============
#data analysis
#==============
#top 5 vehicle types
df %>%
  count(vehicle_type, sort = TRUE) %>%
  top_n(5)

#top 5 items
df %>%
  count(item_name, sort = TRUE) %>%
  top_n(5)

#top 5 item categories names
df %>%
  count(item_category_name, sort = TRUE) %>%
  top_n(5)

#top 5 popular pickup places
df %>%
  count(pickup_place, sort = TRUE) %>%
  top_n(5)

#top 5 place category
df %>%
  count(place_category, sort = TRUE) %>%
  top_n(5)

#top customer id
df %>%
  count(customer_id, sort = TRUE) %>%
  top_n(5)

#top 5 jumpman id
df %>%
  count(jumpman_id, sort = TRUE) %>%
  top_n(5)

#==============
#data visualization
#==============

#how long it took to order vs item name, item category name, pickup place, vehicle type, place category
ggplot(data=df, aes(x=item_name, y=how_long_it_took_to_order)) + geom_bar(stat="identity")




