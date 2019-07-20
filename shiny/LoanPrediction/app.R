#remove old data
rm(list=ls())
#load libraries
#packages
packages <- c("tidyverse",'shiny','shinydashboard')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

