#US eelction
rm(list = ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'caret','dummies','mlbench','tidyr','Matrix','lubridate',
              'data.table','vtreat', 'rsample','scales')
#load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#load data
df <- read.csv("economy_us_election.csv")

# drop downs
presidents <- sort(as.vector(unique(df$President)))
presidential_party <- sort(as.vector(unique(df$Presidential.Party)))
market <- sort(as.vector(unique(df$Market)))


