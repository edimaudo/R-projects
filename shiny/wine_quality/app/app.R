#================================================================================
# Shiny web app which provides insights into wine quality
# using data from UCI Machine learning repository
#================================================================================
rm(list = ls())
#packages 
packages <- c(
              'ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'mlbench','caTools','gridExtra','doParallel','grid',
              'caret','dummies','mlbench','tidyr','Matrix','lubridate',
              'data.table', 'rsample','scales'
              )
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
load("wine_dfR.RData")