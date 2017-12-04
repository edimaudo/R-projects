#server.R
library(shiny)
library(plyr)
library(ggplot2)
source("helper_functions.R")
source("data/example_frequencies.R")

shinyServer(function(input, output) {
  
  output$text <- renderText({
    paste("Original text:", input$text1)
  })
  
  output$freqplot <- renderPlot({
    ref <- switch(input$ref,"English literature classics" = classics,
                  "Oxford dictionary" = oxford,
                  "None" = "None")
    
    fplot(input$text1,ref)
  })
  
  
})