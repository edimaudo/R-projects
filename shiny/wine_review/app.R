#
# Shiny web app that insights into prices, ratings, 
# descriptions, and geographic distribution of the world's most esteemed wines


#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','dummies','ggfortify','shiny',
              'shinydashboard','countrycode','highcharter',"gridExtra")
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
wine_data <- load("wine_dfR.RData")


# Define UI for application that draws a histogram
ui <- fluidPage(
   

)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

