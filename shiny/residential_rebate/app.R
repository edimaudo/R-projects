rm(list = ls())

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'dplyr','readxl')
#load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#load data

#app
ui <- dashboardPage(
    
)

#server info
server <- function(input, output) {
    
}

shinyApp(ui, server)