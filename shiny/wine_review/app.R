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

variety <- as.vector(unique(wine_df$variety))

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
 
      ), 
      tabItem(tabName = "Ratings"),
      tabItem(tabName = "Price"),
      tabItem(tabName = "Variety"),
      tabItem(tabName = "Country"),
      tabItem(tabName = "WineSelector",
      h3("Top 10 wine selections based on Pricing, Rating and Variety"),
      fluidRow(
        box(
          selectInput("variety", 
                      label = "Choose a variety:",
                      choices = variety,
                      selected = "All"),
          radioButtons("pricerange", 
                       label = "Select a price range:",
                       choices = list(#"Any" = 0,
                         "< $10" = 1, 
                         "$10 - $25" = 2, 
                         "$25 - $50" = 3,
                         "$50 - $100" = 4, 
                         "$100 - $500" = 5, 
                         "> $500" = 6), 
                       selected = 1),
          checkboxGroupInput("pointcategory",
                             label = "Select desired rating(s):",
                             choices = list("Classic",
                                            "Superb",
                                            "Excellent",
                                            "Very Good",
                                            "Good",
                                            "Acceptable"),
                             selected = list("Classic",
                                             "Superb",
                                             "Excellent",
                                             "Very Good",
                                             "Good",
                                             "Acceptable")),
          width = 2, 
          height = 600),
        box(DT::dataTableOutput("selected_wines"), width = 10)
      )
      ),
      tabItem(tabName = "WineRecommender")
    )  
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$reviewBox <- renderValueBox({})
  output$tasterBox <- renderValueBox({})
  output$wineBox <- renderValueBox({})
  output$countryBox <- renderValueBox({})
  output$ratingBox <- renderValueBox({})
  output$winePriceBox <- renderValueBox({})
}

# Run the application 
shinyApp(ui = ui, server = server)

