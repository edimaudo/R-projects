#This file contains monthly average electricity usage data in Iowa City from 1971 to 1979.
#questions
#1.	Fit a seasonal trigonometric regression model to the electricity usage data.   
#Forecast the electricity price for January 1980 using the trigonometric regression model.
#2.	Describe in detail two ways of obtaining the seasonally adjusted electricity usage data.
#3.	Obtain the 3rd order moving average for the electricity price data. 
#What is the forecast for April 1980 based on this moving average.
#4.	Using the average electricity usage as the initial value, 
#obtain the next 2 first order exponentially smoothed values using a weight θ of 0.6.   
#Show the detailed calculations step-by-step.

#libraries


rm(list=ls())

#use filechooser to select csv file
iowadata <- read.csv(file.choose(), sep = ',')

#Prepare data
iowadata.orig = iowadata #save orig data copy

iowadata <- na.omit(iowadata) # listwise deletion of missing

#view data
View(iowadata)

#summary
summary(iowadata)

#fit model for 
