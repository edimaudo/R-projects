#remove all data
rm(list=ls())

#libraries
library(readxl)
library(tidyverse)
library(caret)
library(data.table)

#read data
airQ = as.data.frame(read_excel(file.choose())) #excel file

#backup
airQ.orig <- airQ

#sort by wind
airQ <- airQ %>%
  arrange(Wind)

#remove where ozone is NA
airQ <- airQ %>%
  filter(Ozone != "NA")

#Remove the Solar.R column and create a new column containing the ratio of Ozone to Wind.
airQ <- airQ %>%
  select (-Solar.R) %>%
  mutate(Ozone = as.numeric(Ozone)) %>%
  mutate(ozone_wind_ratio = Ozone / Wind) 
  
library(ggplot2)
#For each Temp reading in the original exam1quality data, create a plot of Temp against Ozone
#only if (Temp>=60). In case where the Ozone reading is not available, do NOT plot that Temp-
#Ozone pairing. 
#Ensure that all the plots are on the same axis and include a suitable title thatindicates 
#the month—for example, “Ozone against Temp for Month X.”

ozone_temp <- airQ.orig %>%
  filter(Temp >= 60) %>%
  filter(Ozone != "NA") %>%
  ggplot( aes(x=Ozone, y=Temp, color=Month)) + geom_point() + 
  labs(title="Ozone against Temp for Month X",
       x="Ozone", y = "Temperature") + theme_classic()
print(ozone_temp)
 
#List the Dates in Month/Day format for all readings where Temp > 65 and Wind > 10.3
month_day <- airQ.orig %>%
  filter(Temp > 65 & Wind > 10.3) %>%
  mutate(DateInfo = as.Date( paste(Month , Day , sep = "." )  , format = "%m.%d" ))

#Create a subset of the original exam1quality data containing just three columns: Month, Day,
#and Solar.R. The data should only contain data for June and July. Create a new .csv file
#named solar, and put the data in it.
subset_june_july <-  airQ.orig %>% 
  filter(Month %in% c(6,7)) %>%
  select(Month, Day, Solar.R)

write_csv(subset_june_july, "solar.csv")
  
