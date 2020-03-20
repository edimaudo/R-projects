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

#load data
multipleChoice <- read_csv("multiple_choice_responses.csv")
otherText <- read_csv("other_text_responses.csv")
questions <- read_csv("questions_only.csv")
surveySchema<- read_csv("survey_schema.csv")



ui <- fluidPage(
  navbarPage("2019 Kaggle and Machine Learning Survey",
             tabsetPanel(
               tabPanel("Intro",
                        
                        includeMarkdown("intro.md"),
                        hr()),
               tabPanel("Intro",
                        includeMarkdown("intro.md"),
                        hr()),
               tabPanel("Intro",
                        includeMarkdown("intro.md"),
                        hr())
               
   
             ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

