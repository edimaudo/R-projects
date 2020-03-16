#
# This is a Shiny web application that uses the Kaggle 2019 Data
# Science and Machine learning survery
#
#remove old data
#rm(list=ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 
              'caTools','dummies','ggfortify','shiny')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}




# Define UI for application that draws a histogram
ui <- fluidPage(
   
 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

