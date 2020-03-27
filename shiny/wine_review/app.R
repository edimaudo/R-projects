#
# Shiny web app that insights into prices, ratings, 
# descriptions, and geographic distribution of the world's most esteemed wines
#

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
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Wined"),
  dashboardSidebar(
    width = 275,
    div(img(src = "bottles.jpeg"), style="text-align: center;"),
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("th")),
      menuItem("Background", tabName = "Background", icon = icon("th")),
      menuItem("Ratings", tabName = "Ratings", icon = icon("th")),
      menuItem("Price", tabName = "Price", icon = icon("th")),
      menuItem("Variety", tabName = "Variety", icon = icon("th")),
      menuItem("WineSelector", tabName = "Wine Selector", icon = icon("th")),
      menuItem("WineRecommender", tabName = "Wine Recommender", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Introduction",
              width = 500,
              fluidRow(
                box(width=12,
                  includeMarkdown("intro.md")
                )
              )
      ),
      tabItem(tabName = "Background",
       h1("Background Information") 
      )
    
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

