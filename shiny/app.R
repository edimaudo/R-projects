
################
# Packages
################
rm(list = ls()) #clear environment
packages <- c('ggplot2', 'corrplot','tidyverse','readxl',
              'shiny','shinydashboard','scales','dplyr','mlbench','caTools',
              'forecast','lubridate')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

################
# Load data
################
df <- read_excel("otf.xlsx")


################
# Application UI
################
#------------------
# UI drop-downs
#------------------



################
# Server
################
server <- function(input, output,session) {
  
}

shinyApp(ui, server)