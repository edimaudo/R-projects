#-------------------
#business background
#-------------------
# One very common marketing problem is where & when to invest in order to drive the most conversions with least cost. 
# In our travel business, We are trying to figure out the same problem. 
#In this challenge you will be given a sample data set. 
# Each row represent a visit to our product, 
#and each row has several columns consist of the behavioral & demographic features 
# such as the route they search within this visit, the departure date and the marketing channel that the visit come from, 
# as well as a column indicating how many conversions have happened(0 means no conversion happened).


#business objective
# 1) Please rank the importance of each feature based on their level of correlation to the happening of conversions
# 2) Please identify for route JKT(Jakarta) - DPS(Bali), which channel x device x travel time x any 
# other significant influencing feature we should invest in most in order to drive most bookings out of this route
# 3) Please build a machine learning model that takes all columns (can also choose features) except “conversions” as x 
#and “conversions” as y 
#to predict whether a booking will happen given all column x

#data dictionary

# data_created_at: Time when the visit to our product(i.e. web, app) happens
# 
# device: The device that makes this visit, i.e. desktop, smartphone
# 
# Os_type: The operation of the visiting device, i.e. iOS, android, windows
# 
# Client_type: product type, either web or app
# 
# Airlines: The two letter code of the airlines that the user select to fly with. If it’s round trip, 
#the outbound and inbound airlines are separated by “=”. 
#If the user changes airline within any leg, 
#(i.e. a user’s final destination is hong kong but stopover at KL and changes an airline), 
#then the two segment airline would be separated by “-” 
# 
# Stops: With stopover or without stopover
# 
# Trip_type: oneway or roundtrip
# 
# Trip_category: domestic or international
# 
# Cabin_class: EONOMY, PREMIUM ECONOMY OR BUSINESS
# 
# Adults_count: Number of adults
# 
# Children_count: Number of children
# 
# Infants_count: Number of infants
# 
# Total_price_usd: Total value of tickets that the user select, with 5 decimal points
# 
# User_city: Where the user is when he/she conducts the product visit
# 
# Channel: The marketing channel that the user come from
# 
# Device_resolution: i.e. 1080 x 1794
# 
# Device_version: Model of the visiting device i.e. Samsung G930F
# 
# Os_version: Operating system of the device i.e. iOS 8.0
# 
# App_version: app version, the value is null if client type = web
# 
# Network_type: 4g, wifi or 3g
# 
# Network_carrier_name: Telecom company that hosts the internet connection, i.e. Singtel
# 
# Conversions: Number of goal happened

#remove old data
rm(list=ls())

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse',
                  "cowplot",'lubridate','data.table','car','caret','mlbench','xgboost','plotrix')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.csv(file.choose())

#backup data
df.backup <- df

#review data
print(summary(df))

#missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data) #no missing data

#1) 
#look for significant correlations
corrinfo <- df %>%
  select(conversions,adults_count, children_count, infants_count,total_price_usd)
corrplot(cor(corrinfo), method = "number")
#none of the numerical values has any strong correlation

#2)
# Please identify for route JKT(Jakarta) - DPS(Bali), which channel x device x travel time x any 
# other significant influencing feature we should invest in most in order to drive most bookings out of this route

routeJKTDSP <- df %>%
  filter(departure_city_code == "JKT", arrival_city_code == "DPS")

routeinfo <- routeJKTDSP %>%
  select(conversions, device, os_type, client_type, channel, device_version, app_version)

#library(MASS)
#scatterplot.matrix(routeinfo)

#device vs conversions
ggplot(data = df,aes(x=as.factor(device),y=conversions)) + geom_bar(stat="identity") + 
  theme_classic() +  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  xlab("device") + ylab("Conversions") #smartphone had the highest conversion numbers

#os type vs conversions
ggplot(data = df,aes(x=as.factor(os_type),y=conversions)) + geom_bar(stat="identity") + 
  theme_classic() +  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  xlab("os type") + ylab("Conversions") # android and ios had the highest conversions which 
#Åis in line with device info above

#client type vs conversions
ggplot(data = df,aes(x=as.factor(client_type),y=conversions)) + geom_bar(stat="identity") + 
  theme_classic() +  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  xlab("Client type") + ylab("Conversions") #inline with os and device type

#app versions vs. conversions
ggplot(data = df,aes(x=as.factor(app_version),y=conversions)) + geom_bar(stat="identity") + 
  theme_classic() +  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  xlab("app versions") + ylab("Conversions") #app versions 5.5.2 and 5.6.9 had very high conversions

#channel type vs. conversions
ggplot(data = df,aes(x=as.factor(channel),y=conversions)) + geom_bar(stat="identity") + 
  theme_classic() +  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  xlab("Channel") + ylab("Conversions") #mobile app had the highest conversion

#mobile users have the highest conversion rate

#3) build predictive model

df_main_non_category <- df %>%
  select(total_price_usd, conversions) %>%
  na.omit()

df_main_category <- df %>%
  select(device, os_type, client_type,channel, stops, trip_type, trip_category, cabin_class) %>%
  na.omit()

##transform categorical variables to
library(dummies)
df_main_category.new <- dummy.data.frame(df_main_category, sep = ".")
#drop columns to avoid multicollinearity
df_main_category.new$device.desktop <- NULL
df_main_category.new$os_type. <- NULL
df_main_category.new$client_type.api <- NULL
df_main_category.new$`channel.affiliates-api` <- NULL
df_main_category.new$stops. <- NULL
df_main_category.new$trip_type.multicity <- NULL
df_main_category.new$cabin_class.ECONOMY <- NULL

df_main <- cbind(df_main_category.new, df_main_non_category)

#feature selection
#Remove Redundant Feature remove when absolute correlation >= 0.75
set.seed(7)
# # calculate correlation matrix
correlationMatrix <- cor(df_main[,1:25])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#based on this remove these columns 8 13  9  1 20  3 18 16

#drop unimportant features
df_main[,20] <- NULL
df_main[,18] <- NULL
df_main[,16] <- NULL
df_main[,13] <- NULL
df_main[,9] <- NULL
df_main[,8] <- NULL
df_main[,3] <- NULL
df_main[,1] <- NULL

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_main$total_price_usd <- normalize(df_main$total_price_usd)

#convert conversions to factor
df$conversions <- as.factor(df$conversions)

#machine learning
#split data in test and train
set.seed(123)
library(caTools)
df_main$conversions <- as.factor(df_main$conversions)
sample <- sample.split(df_main,SplitRatio = 0.75)
train <- subset(df_main,sample ==TRUE)
test <- subset(df_main, sample==FALSE)

#models
model_gbm<-train(train[,1:17],train[,18],method='gbm')
model_rf<-train(train[,1:17],train[,18],method='rf')
model_nnet<-train(train[,1:17],train[,18],method='nnet')



