rm(list = ls())
##################
# packages 
##################
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard','readxl',
              'scales','dplyr','mlbench','caTools',
              'forecast','lubridate','tidytext',
              'SnowballC','wordcloud', 'RColorBrewer')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

##################
# data
##################
df <- read.csv("clean.csv")


##################
# UI
##################


##################
# Server
##################
server <- function(input, output) {
    
}


shinyApp(ui, server)