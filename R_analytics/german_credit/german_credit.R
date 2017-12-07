#libraries
library(ggplot2)
library(corrplot)
library(lattice)
library(dplyr)
library(tidyr)
library(LogicReg)
library(caret)
library(readxl)

#==========================
#logistic regression
#==========================

#Clear old data
rm(list=ls())

mydata <- read_excel(file.choose())

#summary
summary(mydata)

#Prepare data
mydata.orig = mydata #save orig data copy

mydata <- na.omit(mydata) # listwise deletion of missing

#set seed
set.seed(1)