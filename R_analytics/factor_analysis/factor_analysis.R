#For the dataset provided, do the following:

#Perform factor analysis for all variables to determine the top 4 most unique variables 
#using 12 factors

#Determine the minimum number of factors needed given a 90% confidence level

#Provide the scores of all subjects in the top 10 factors 
#(performing factor analysis using 15 factors)

#-need to use the "factanal" library

#remove all data
rm(list=ls())

#load libraries
library(psy)
library(nFactors)
library(MASS)
library(psych)
library(GPArotation)
library(cluster)
library(ggplot2)
library(factoextra)
library(corrplot)
library(lattice)
library(NbClust)