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

#gender

#Age

#Country

#Education

ui <- fluidPage(
  navbarPage("2019 Kaggle and Machine Learning Survey",
             tabsetPanel(
               tabPanel("Intro",
                        includeMarkdown("intro.md"),
                        hr()),
               tabPanel("Key Insights",
                        h1("Data Scientist Profile"),
                        h3("Gender"),
                        h3("Age"),
                        h3("Country"),
                        h1("Education"),
                        h3(""),
                        h3("Data Science & Machine learning experience"),
                        
                        hr()),
               tabPanel("Contry Comparison",
                       
                        hr())
               
   
             ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

