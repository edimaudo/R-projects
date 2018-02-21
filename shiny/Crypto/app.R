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

newdata <- mydata[order(mydata$name),] 

symbolInfo <- unique(newdata$name)

tickers = c("Bitcoin","Ethereum","Ripple","Bitcoin Cash","Bitcoin Gold","Litecoin")


# Define UI for application crypto app
ui <- fluidPage(
  titlePanel("Cryptocurrency prediction"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("nameInfo", "Crypotcurrency:", 
                  choices=symbolInfo)
    ),
    
    # Create a spot for the prediction
    mainPanel(
      plotOutput("CryptocurrencyPrediction")  
    )
  )
)

# Define server logic for crypto prediction app
server <- function(input, output) {

  output$CryptocurrencyPrediction <- renderPlot({
    
    #get the data
    graphdata <- mydata %>%
      filter(name %in% input$nameInfo) %>%
      select(date, close)
    
    #render the forecast
    graphdata1 <- ts(graphdata, start=2013, end=2025, frequency = 12)
    plot(forecast(graphdata1[,2]), col="grey", main = input$nameInfo, xlab="Date", ylab="Closing Prices")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

