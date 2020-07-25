
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard','scales')
#load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#load data
df <- read_csv("applemobilitytrends-2020-07-23.csv")

region_info <- sort(unique(df$region))
transportation_type_info <- sort(unique(df$transportation_type))

