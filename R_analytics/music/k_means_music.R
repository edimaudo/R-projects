#clear old data
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
library(readxl)

#get data
mydata = as.data.frame(read_excel(file.choose()))