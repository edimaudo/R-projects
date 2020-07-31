#clear environment
rm(list = ls())

#packages 
packages <- c('tidyverse','dplyr', 'readxl', 'ggplot2')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_excel("Customer data.xlsx")

