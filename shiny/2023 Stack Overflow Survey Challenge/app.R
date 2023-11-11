## 2023 StackOverflow Survey Analysis

## Environment setup
rm(list = ls())

## Load Libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret",
              'scales','ggfortify','DT',
              'shiny','shinydashboard','lubridate','caret',
              'mlbench','mice','countrycode','highcharter',"gridExtra",
              'stopwords','tidytext','stringr','TTR','xts',
              'reshape2', 'textmineR','topicmodels','textclean')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

## Load Data
df <- read_csv("survey_results_public.csv")

## UI dropdowns


## UI 


## Server