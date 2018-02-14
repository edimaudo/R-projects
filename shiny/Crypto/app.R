#
#to do
# use forecasting tools tbat or other
# perform visualization + UI build
#

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(forecast)


#read in the data
mydata <- read.csv("/Users/edima/Documents/Coding/R/R_analytics/cryptocurrency_prediction/crypto-markets.csv")

symbolInfo <- unique(mydata$name)
symbolInfo <- symbolInfo %>%
  arrange(symbolInfo)

tickers = c("Bitcoin","Ethereum","Ripple","Bitcoin Cash","Bitcoin Gold","Litecoin")



# Define UI for application crypto app
ui <- fluidPage(
  titlePanel("Cryptocurrency prediction"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("currencyInfo", "Crypotcurrency:", 
                  choices=symbolInfo)
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("Cryptocurrency prediction")  
    )
  )
)

# Define server logic for crypto prediction app
server <- function(input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)

