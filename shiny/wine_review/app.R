# review data - 
# review what other people did - 
# design what visualization is needed 
# design readme - 
# design about section for the app - 
# Review libraries to use - 
# Design the layout of the app - 
# build initial layout - 
#build introduction - 
#create reminaing tabs layout - 
#build background - add it with centered layout - 
#use boxes and summary information layout for the background - 
#wine selector varietal + price + rating + top 10 outputs table layout - 
#have rating drop down + visualization for prcies, country & variety layout - 
#have price drop down + visualization for rating, country & variety layout - 
#have variety checkboxes + visualization for rating, country, prices layout - 
#debug issues for layouts + test - sat - 
#wine selector varietal + price + rating + top 10 outputs table layout 

#wine selector varietal + price + rating + top 10 outputs table code 

#use boxes and summary information layout + code + test 

#have rating drop down + visualization for prcies, country & variety code + test
#have price drop down + visualization for rating, country & variety code + test
#have variety dropdown + visualization for rating, country, prices code + test

#wine recommendation using user layout + recommendation code + test


#
# Shiny web app that insights into prices, ratings, 
# descriptions, and geographic distribution of the world's most esteemed wines
#

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny',
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

variety <- as.vector(unique(wine_df$variety))

# Define UI for application that draws a histogram
ui <- fluidPage(
   

)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

