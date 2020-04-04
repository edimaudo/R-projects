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

# Define UI for wine app
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
      menuItem("Country", tabName = "Country", icon = icon("th")),
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
       h3("Background Information"),
       fluidRow(
         valueBoxOutput("reviewBox"),
         valueBoxOutput("tasterBox"),
         valueBoxOutput("wineBox"),
         valueBoxOutput("countryBox"),
         valueBoxOutput("ratingBox"),
         valueBoxOutput("winePriceBox")
       )
      ),tabItem(tabName = "Ratings",
          h1("Ratings Information"),
          fluidRow(
            box(selectInput("ratingInput", 
                        label = "Ratings",
                        choices =c("","")), height = 100,width = 25)),br(),
            box(h3("Price"),
              plotOutput("priceRatingplot", height = 200, width = 5)),
            box(h3("Variety"),
                plotOutput("varietyRatingplot", height = 200,width = 5)),
            box(h3("Country"),
                plotOutput("countryRatingplot", height = 200,width = 5))
          )
      )
      )
    )
  


# Define server logic for the wine app
server <- function(input, output) {
   
  # output$reviewBox <- renderValueBox({})
  # output$tasterBox <- renderValueBox({})
  # output$wineBox <- renderValueBox({})
  # output$countryBox <- renderValueBox({})
  # output$ratingBox <- renderValueBox({})
  # output$winePriceBox <- renderValueBox({})
}

# Run the application 
shinyApp(ui = ui, server = server)

