#remove old data
rm(list=ls())

#packages
packages <- c('tidyverse','DataExplorer','fastDummies','mlbench','caret','caTools')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.table(file.choose(), sep=";", header=T) #"bank-full.csv"

