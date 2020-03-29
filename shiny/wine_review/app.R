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
      sidebarPanel(
        helpText("Select Ratings from the dropdown"),
        selectInput("ratingInput", "Ratings",choices=c("","")),
        br()
      ),
      mainPanel(
        h1("Ratings Information"),
        h3("Price"),
        plotOutput("priceRatingplot"),
        h3("Variety"),
        plotOutput("varietyRatingplot"), 
        h3("Country"),
        plotOutput("countryRatingplot")
      ),
      tabItem(tabName = "Price"),
      sidebarPanel(
        helpText("Select Prices from the dropdown"),
        selectInput("priceInput", "Prices",choices=c("","")),
        br()
      ),
      mainPanel(
        h1("Price Information"),
        h3("Rating"),
        plotOutput("ratingPriceplot"),
        h3("Variety"),
        plotOutput("varietypPricelot"), 
        h3("Country"),
        plotOutput("countryPriceplot")
      ),
      tabItem(tabName = "Variety"),
      sidebarPanel(
        helpText("Select Variety from the dropdown"),
        selectInput("varietyInput", "Varieties",choices=c("","")),
        br()
      ),
      mainPanel(
        h1("Varieties Information"),
        h3("Price"),
        plotOutput("priceVarietyplot"),
        h3("Rating"),
        plotOutput("ratingVarietyplot"), 
        h3("Country"),
        plotOutput("countryVarietyplot")
      ),
      tabItem(tabName = "Country"),
      sidebarPanel(
        helpText("Select Country from the dropdown"),
        selectInput("countryInput", "Ratings",choices=c("","")),
        br()
      ),
      mainPanel(
        h1("Country Information"),
        h3("Price"),
        plotOutput("priceCountryplot"),
        h3("Rating"),
        plotOutput("ratingCountryplot"), 
        h3("Variety"),
        plotOutput("varietyCountryplot")
      ),
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

