#bill board analysis

#clear old data
rm(list=ls())

#load libraries
packages <- c('ggplot2', 'corrplot', 'data.table', 'lubridate',
              'stringr', "tidytext","tidyverse", "mlbench", "caret", "caTools")

#load libraries
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data

