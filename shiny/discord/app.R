#================================================================================
# Shiny web app which provides insights into Ocean Protocol's Discord server, 
# providing a deeper understanding of the community's dynamics and predicting future activity pattern
#================================================================================
rm(list = ls())
################
#packages 
################
packages <- c(
  'ggplot2', 'corrplot','tidyverse','shiny','shinydashboard','DT',
  'mlbench','caTools','gridExtra','doParallel','grid','forecast',
  'caret','dummies','mlbench','tidyr','Matrix','lubridate',
  'data.table', 'rsample','scales','stopwords','tidytext','stringr',
  'reshape2', 'textmineR','topicmodels','textclean'
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
df<- read.csv("Ocean Discord Data Challenge Dataset.csv")


################
#UI
################
ui <- dashboardPage(
  dashboardHeader(title = "Ocean Discord Data Challenge"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("Overview", tabName = "overview", icon = icon("th")),
      menuItem("Wine Quality Insights", tabName = "prediction", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",includeMarkdown("readme.md"),hr()),
      tabItem(tabName = "overview",
              sidebarLayout(
                sidebarPanel(
                  selectInput("wineTypeInput", "Wine Type", choices = c("Red","White"), selected = "Red"),
                  submitButton("Submit")
                ),
                mainPanel(
                  h3("Wine Quality distribution",style="text-align: center;"),
                  plotOutput("WineQualityPlot"),
                  h3("Wine Properties Correlation",style="text-align: center;"),
                  plotOutput("WineCorrelationPlot"),  
                  h3("Quality vs pH",style="text-align: center;"),
                  plotOutput("QualitypHPlot"),
                  h3("Quality vs Alcohol Content",style="text-align: center;"),
                  plotOutput("QualityAlcoholPlot")
                )
              )
      )
    )
   )
  ) 
 
