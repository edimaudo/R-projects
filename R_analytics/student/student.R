#objective
#-Summarise and visualise the data.
#-Prepare the dataset for analysis (data cleansing, choose appropriate features to model)
#-Build model to predict final result

#data dictionary
# code_module – an identification code for a module on which the student is registered.
# code_presentation - the identification code of the presentation during which the 
#                      student is registered on the module.
# id_student – a unique identification number for the student.
# gender – the student’s gender.
# region – identifies the geographic region, where the student lived 
#          while taking the module-presentation.
# highest_education – highest student education level on entry to the module presentation.
# imd_band – specifies the Index of Multiple Depravation band of the place where the 
#            student lived during the module-presentation.
# age_band – band of the student’s age.
# num_of_prev_attempts – the number times the student has attempted this module.
# studied_credits – the total number of credits for the modules the student is currently studying.
# disability – indicates whether the student has declared a disability.
# final_result – student’s final result in the module-presentation.

# remove old data
rm(list=ls())

#libraries
library(corrplot)
library(tidyverse)
library(lubridate)
library(mlbench)
library(caret)
library(xgboost)
library(ggplot2)

#read csv file
df <- read.csv(file.choose())

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data) #no missing data

#keep backup of data
df.old <- df

#remove na's
df <- na.omit(df)

#================
#visualize data
#================

#gender vs. region
ggplot(data=df, aes(x=region, y=gender, fill=gender)) +
  geom_bar(stat="identity") + theme_classic()

#gender vs. highest education
ggplot(data=df, aes(x=region, y=final_result, fill=highest_education)) +
  geom_bar(stat="identity") + theme_classic()


#================
#business review
#================
#things to remove
#- code module
#-id_student
#-imd_band

#================
#remove unneeded columns
#================
df$code_module <- NULL
df$imd_band <- NULL

#================
#predictive model
#================
#split data into training and test
library(caTools)
set.seed(123)
sample <- sample.split(df,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train <- subset(df,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test <- subset(df, sample==FALSE)