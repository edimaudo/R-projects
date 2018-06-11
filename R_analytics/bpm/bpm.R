#clear old data
rm(list=ls())

#load libraries
library(ggplot2)
library(corrplot)
library(tidyverse)
library(forecast)

#load data
df <- read.csv(file.choose(), sep=";")

#View(df)
