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
      tabItem(
          tabName = "Background",
         h3("Background Information"),
         fluidRow(
           valueBoxOutput("reviewBox"),
           valueBoxOutput("tasterBox"),
           valueBoxOutput("wineBox"),
           valueBoxOutput("countryBox"),
           valueBoxOutput("ratingBox"),
           valueBoxOutput("winePriceBox")
         )
      ),tabItem(
              tabName = "Ratings",
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
      ),tabItem(
                tabName = "Price",
                h1("Price Information"),
                fluidRow(
                  box(selectInput("priceInput", 
                                  label = "Prices",
                                  choices =c("","")), height = 100,width = 25)),br(),
                box(h3("Rating"),
                    plotOutput("ratingPriceplot", height = 200, width = 5)),
                box(h3("Variety"),
                    plotOutput("varietyPriceplot", height = 200,width = 5)),
                box(h3("Country"),
                    plotOutput("countryPriceplot", height = 200,width = 5))
      ),tabItem(
                tabName = "Variety",
                h1("Variety Information"),
                fluidRow(
                  box(selectInput("varietyInput", 
                                  label = "Variety",
                                  choices =c("","")), height = 100,width = 25)),br(),
                box(h3("Rating"),
                    plotOutput("ratingVarietyplot", height = 200, width = 5)),
                box(h3("Price"),
                    plotOutput("priceVarietyplot", height = 200,width = 5)),
                box(h3("Country"),
                    plotOutput("countryVarietyplot", height = 200,width = 5))
      ), tabItem(
                tabName = "Country",
                h1("Country Information"),
                fluidRow(
                  box(selectInput("countryInput", 
                                  label = "Country",
                                  choices =c("","")), height = 100,width = 25)),br(),
                box(h3("Rating"),
                    plotOutput("ratingCountryplot", height = 200, width = 5)),
                box(h3("Price"),
                    plotOutput("priceCountryplot", height = 200,width = 5)),
                box(h3("Variety"),
                    plotOutput("varietyCountryplot", height = 200,width = 5))        
      ),tabItem(tabName = "WineSelector",
                h3("Select the right wine for you"),
                fluidRow(
                  box(
                    selectInput("variety", 
                                label = "Choose a variety:",
                                choices = c("",""),
                                selected = "All"),
                    radioButtons("priceRange", 
                                 label = "Select a price range:",
                                 choices = list(#"Any" = 0,
                                   "< $10" = 1, 
                                   "$10 - $25" = 2, 
                                   "$25 - $50" = 3,
                                   "$50 - $100" = 4, 
                                   "$100 - $500" = 5, 
                                   "> $500" = 6), 
                                 selected = 1),
                    checkboxGroupInput("pointCategory",
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
                  box(DT::dataTableOutput("selected_wines"), width = 10))        
      ), 
      tabItem(
        tabName = "WineRecommender"
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

