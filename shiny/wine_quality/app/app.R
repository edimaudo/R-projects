#================================================================================
# Shiny web app which provides insights into wine quality
# using data from UCI Machine learning repository
#================================================================================
rm(list = ls())
################
#packages 
################
packages <- c(
              'ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'mlbench','caTools','gridExtra','doParallel','grid',
              'caret','dummies','mlbench','tidyr','Matrix','lubridate',
              'data.table', 'rsample','scales'
              )
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

################
#Load data
################
red_df <- read.csv("winequality-red.csv",sep=";")
white__df <- read.csv("winequality-white.csv",sep=";")

################
#Define UI for application
################
ui <- dashboardPage(
  dashboardHeader(title = "Wine Quality Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("Overview", tabName = "overview", icon = icon("th")),
      menuItem("Wine Quality Prediction", tabName = "prediction", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",includeMarkdown("about.md"),hr()),
      tabItem(tabName = "overview",
              sidebarLayout(
                sidebarPanel(
                  selectInput("wineTypeInput", "Wine Type", choices = c("Red","White"), selected = "Red"),
                  submitButton("Submit")
                ),
                mainPanel(
                  h2("Wine Quality Overview",style="text-align: center;"), 
                  fluidRow(
                    h3("Wine Quality distribution",style="text-align: center;"),
                    
                    h3("Wine Properties Correlation",style="text-align: center;"),
                    
                    h3("Quality vs pH",style="text-align: center;"),
                    
                    h3("Quality vs Alchol Content",style="text-align: center;"),
                  )
                )
              )
      ),
      tabItem(tabName="prediction",
              mainPanel(
                h2("Wine Quality Prediction",style="text-align:center;")
              )
              )
    )
  )
)


      
################
# Define server logic 
################
server <- function(input, output,session) {
  
}

shinyApp(ui, server)