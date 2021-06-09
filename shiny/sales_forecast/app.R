# Objective is to forecast Revenue

rm(list = ls()) #clear environment

#===============
# libraries
#===============
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl','scales',
              'dplyr','mlbench','caTools','forecast','TTR','xts','lubridate','shiny','shinydashboard')
# load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#===============
# Load data
#===============
df <- read.csv("volume_by_day_type_and_region.csv")

#===============
# Data update
#===============
df$date <- lubridate::as_date(df$date) #update date
df$revenue <- df$average_price*df$total_volume #revenue

#===============
# Drop downs
#===============
type_info <- sort(as.vector(unique(df$type))) #type 
type_info <- c(type_info,"All")
region_info <- sort(as.vector(unique(df$region))) #region
region_info <- c(region_info,"All")
horizon_info  <- c(1:100) #forecast range
aggregate_info <- c('weekly','monthly')
model_info <- c('auto arima','auto exponential','simple exponential',
                'double exponential','triple exponential')

#===============
# Define UI for application
#===============
ui <- dashboardPage(
  dashboardHeader(title = "Forecasting Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("dashboard")),
      menuItem("Sales forecast", tabName = "forecast", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",includeMarkdown("readme.md"),hr()),
      tabItem(tabName = "forecast",
              sidebarLayout(
                sidebarPanel(
                  selectInput("aggregateInput", "Aggregate", choices = aggregate_info, selected = 'weekly'),
                  selectInput("typeInput", "Type", choices = type_info,selected = "All"),
                  selectInput("regionInput", "Region", choices = region_info, selected = "All"),
                  selectInput("forecasthorizonInput", "Forecast Horizon", choices = horizon_info, selected = 12),
                  selectInput("modelInput", "Model", 
                              choices = model_info, selected = 'auto exponential'),
                  submitButton("Submit")
                ),
                mainPanel(
                  h2("Revenue Forecast Analysis",style="text-align: center;"), 
                  fluidRow(
                    h3("Forecast Plot",style="text-align: center;"),
                    plotOutput("forecastPlot"),
                    br(),
                    h3("Forecast Accuracy",style="text-align: center;"),
                    DT::dataTableOutput("accuracyOutput")
                  )
                )
              )
      ) 
    )
  )
)

#===============
# Define server logic 
#===============
server <- function(input, output,session) {
  
  #forecast output
  output$forecastPlot <- renderPlot({
    
  })
  
  # accuracy output
  output$accuracyOutput <- DT::renderDataTable({
  
  })
  
}

shinyApp(ui, server)



