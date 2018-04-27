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

#load nps data
df <- read.csv(file.choose(), sep=";")