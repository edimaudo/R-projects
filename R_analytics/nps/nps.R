#nps - net promoter score

#remove all data
rm(list=ls())

#libraries
library(readxl)
library(tidyverse)
library(caret)
library(mlbench)
library(data.table)
library(ggplot2)
library(corrplot)
library(pastecs)

#load nps data
df <- read.csv(file.choose(), sep=";")

#view data
glimpse(df)

#check for missing data
apply(df, 2, function(x) any(is.na(x))) #no missing data

#set seed for reproducability
set.seed(123)

#descriptive statistics
summary(df)
stat.desc(df) 

# graphs
# Build bar chart that has 
# region vs account
# region vs product
# region vs BU

# Contigency analysis - Specify hypotheses; 
#Run two Chi2-Tests of Independence: (1) BU and NPS categories; (2) Region and NPS categories;
 
# Group comparison - Specify hypotheses; 
#Conduct two t-tests (hypotheses tests). Dependent variable is REC (NPS raw score).
#(1st t-test) Independent variable is Salesman.Professionality > 7 
#(built a new dummy variable first!􏰀0 if Salesman.Professionality <= 7, 1 if >= 8)
#; (2nd t-test) Independent variable is Product.Tastiness > 7 
#(built a new dummy variable first! – use same rules as before);

# correlation analysis
 

# multiple linear regression - dependdent variable is REC
# outliers, non normality
# note - use either BU or PRODUCT but not both
# note -  check linear regression assumptions
# - split data into test and train
# - initial model using train data
# - find important variables
# - build new model and test
# - get AIC, accuracy