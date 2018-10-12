#Problem Statement
#A leading affiliate network company from Europe wants to leverage machine learning to 
#improve (optimise) their conversion rates and eventually their topline. 
#Their network is spread across multiple countries in europe such as Portugal, 
#Germany, France, Austria, Switzerland etc.

#Affiliate network is a form of online marketing channel where an intermediary promotes 
#products / services and earns commission based on conversions (click or sign up). 
#The benefit companies sees in using such affiliate channels is that, 
#they are able to reach to audience which doesn’t exist in their marketing reach.
#The company wants to improve their CPC (cost per click) performance. 
#A future insight about an ad performance will give them enough headstart 
#to make changes (if necessary) in their upcoming CPC campaigns.

#Objective
#In this challenge, you have to predict the probability whether an ad will get clicked or not.


#Data Description
#You are given three files to download: train.csv, test.csv and sample_submission.csv 
#Variables in this data set are anonymized due to privacy. 
#The training data is given for 10 days ( 10 Jan 2017 to 20 Jan 2017). 
#The test data is given for next 3 days.

# Variable	Description
# ID	Unique ID
# datetime	timestamp
# siteid	website id
# offerid	offer id (commission based offers)
# category	offer category
# merchant	seller ID
# countrycode	country where affiliates reach is present
# browserid	browser used
# devid	device used
# click	target variable

#clear old data
rm(list=ls())

#load libraries
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools', 
              'MASS','Metrics','randomForest','lars','xgboost','Matrix','methods', 'lubridate',
              'data.table')

#load libraries
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
train <- read.csv(file.choose())
test <- read_csv(file.choose())
sample_submission <- read.csv(file.choose())

#create backup
train.backup <- train

#review train data
glimpse(train)



#perform data cleaning

#perform data transformation

#data modelling


