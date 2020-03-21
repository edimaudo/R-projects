#
# This is a Shiny web application that uses the Kaggle 2019 Data
# Science and Machine learning survey
#

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
                        h1("Data Scientist Profile",style = "color:blue"),
                        h3("Gender"),
                        h3("Age"),
                        h3("Country"),
                        h1("Education",style = "color:blue"),
                        h3("Education degrees"),
                        h3("Learning Channels"),
                        h1("Data Science & Machine learning experience",
                           style ="color:blue"),
                        h3("Time spent learning to code"),
                        h3("Time spend learning machine learning"),
                        h1("Employment",style = "color:blue"),
                        h3("Pay"),
                        h3("Time spent"),
                        h3("Company information"),
                        h3("Data Science teams"),
                        h3("Enterprise machine learning adoption"),
                        h3("Spending"),
                        h1("Technology",style = "color:blue"),
                        h3("IDE usage"),
                        h3("Methods and algorithms usage"),
                        h3("Framework usage"),
                        h3("Enterprise tools usage"),
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

