#================================================================================
# Shiny web app which provides insights into Ocean Protocol's Discord server, 
# providing a deeper understanding of the community's dynamics and predicting future activity pattern
#================================================================================
rm(list = ls())
################
#packages 
################
packages <- c(
  'ggplot2', 'corrplot','tidyverse','shiny','shinydashboard','DT',
  'mlbench','caTools','gridExtra','doParallel','grid',
  'caret','dummies','mlbench','tidyr','Matrix','lubridate',
  'data.table', 'rsample','scales'
)
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

################
#Load data
################
df<- read.csv("Ocean Discord Data Challenge Dataset.csv")