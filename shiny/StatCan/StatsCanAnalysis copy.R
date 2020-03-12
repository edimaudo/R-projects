#remove old data
rm(list=ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 
              'caTools','dummies','ggfortify)')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


bigfile.raw <- tbl_df(read.csv("Small_Version/Small_aggregate/2019_all.csv", 
                               stringsAsFactors=FALSE, header=T,nrow=10000000, comment.char="")) 
write.csv(bigfile.raw,'OUTPUT_main.csv',row.names = TRUE)

#summary view of data
glimpse(bigfile.raw)

#unique(bigfile.raw$Province)

#Visualizations of data
# ggplot(bigfile.raw, aes(x=Province))+
#   geom_bar(stat="count", width=0.7, fill="steelblue")+
#   theme_classic()

#load google trends data
library("readxl")
# xlsx files
trend_data <- read_excel("gtrend.xlsx")
summary(trend_data)
glimpse(trend_data)


install.packages('gtrendsR')
library (gtrendsR)



scan_data <- bigfile.raw
# Rename the variables
colnames(trend_data) <- c("Product","Province","Requests")
colnames(scan_data) <- c("Week","WeekNo","StoreNo","City",'Province',"DisseminationArea",
                         "ProductCode","Product","NAPCS","Price","Quantity",'Sales')

#convert Sales to numeric
scan_data$Sales <- as.numeric(as.character(scan_data$Sales))
scan_data <- na.omit(scan_data)

#aggregate scan data for 2019
# scan_aggregate_data <- scan_data %>% 
#   select("Product",'Province','Sales')
# 
# scan_aggregate_data2 <- scan_aggregate_data %>%
#   group_by("Product",'Province') %>%
#   summarise(sum_Sales = sum("Sales"))
# 
# write.csv(scan_aggregate_data,'summary.csv',row.names = TRUE)

#incorporate google trends data
scan_aggregate_data <- read_excel("Product_province_sales.xlsx")

glimpse(scan_aggregate_data)
colnames(scan_aggregate_data) <- c("Product","Province","Sales")

scan_trend_data <- scan_aggregate_data %>% inner_join(trend_data, by = c("Province","Product"))
summary(scan_trend_data)
glimpse(scan_trend_data)

#visualization
#Requests

# Requests by Province
p<-ggplot(data=scan_trend_data, aes(x=Province, y=Requests)) +
  geom_bar(stat="identity")

#Requests by Products and Province
# Stacked barplot with multiple groups
ggplot(data=scan_trend_data, aes(x=Province, y=Requests, fill=Product)) +
  geom_bar(stat="identity")

# Use position=position_dodge()
#ggplot(data=scan_trend_data, aes(x=Province, y=Requests, fill=Product)) +
#  geom_bar(stat="identity", position=position_dodge())

#Sales by Product and Province
ggplot(data=scan_trend_data, aes(x=Province, y=Sales, fill=Product)) +
  geom_bar(stat="identity")
