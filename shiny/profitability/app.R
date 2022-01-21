##################
# packages 
##################
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'scales','dplyr','mlbench','caTools',
              'forecast','lubridate')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

##################
# data
##################
df <- read.csv("Profitability.csv")

#----------------
# data updates
#----------------
df$DateComplete <- as.Date(df$DateComplete, format =  "%d/%m/%Y")
df$DateLogged <- as.Date(df$DateLogged, format =  "%d/%m/%Y")

##################
# UI
##################

#----------------
# UI dropdown
#----------------
date_info <- c(sort(unique(df$DateLogged)))




##################
# Server
##################
server <- function(input, output, session) {}


