#=========
# Mobile Phone Ratings
#=========

packages <- c('ggplot2', 'corrplot','tidyverse','readxl',
              'shiny','shinydashboard','scales','dplyr','mlbench','caTools',
              'forecast','TTR','xts','lubridate','plotly')

for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#=========
# Load data
#=========
df <- read.csv("mobile phone rating.csv")

#----------
# UI Dropdown
#----------


#=========
# UI
#=========


#=========
# Server
#=========
server <- function(input, output,session) {}

shinyApp(ui, server)