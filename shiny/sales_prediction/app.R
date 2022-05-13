################
# Sales Insights
################

rm(list = ls()) # clear environment

################
# Libraries
################
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl','scales',
              'dplyr','mlbench','caTools','forecast','TTR','xts','lubridate','shiny',
              'shinydashboard')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

################
# Load Data
################
df <- read_excel("Sales.xlsx")

#===============
# Data Munging
#===============

################
# Application UI
################

item_code_dropdown <- sort(as.vector(unique(df$`Item Code`))) # Item code 
item_code_dropdown <- c("All",item_code_dropdown)
country_dropdown <- sort(as.vector(unique(df$Country))) # Country
country_dropdown<- c("All",country_dropdown)
horizon_info  <- c(1:100) #forecast range
aggregate_info <- c('daily','weekly','monthly')
frequency_info <- c(7, 12, 52, 365)
model_info <- c('auto arima','auto exponential','simple exponential',
                'double exponential','triple exponential')

ui <- dashboardPage(
    dashboardHeader(title = "Sales Insights & Prediction"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data", tabName = "data", icon = icon("th")),
            menuItem("Analysis", tabName = "analysis", icon = icon("th")),
            menuItem("Forecasting", tabName = "Forecast", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "data",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("itemCodeInput", "Item Code", choices = item_code_dropdown),
                            selectInput("countryInput", "Country", choices = country_dropdown),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Analysis",style="text-align: center;"), 
                            # Sales Trend
                            
                            # Sales Info
                            tabsetPanel(type = "tabs",
                                        tabPanel(
                                            h4("Top 5 Quantity by Country",
                                               style="text-align: center;"),
                                            plotOutput("decompositionPlot")),
                                        tabPanel(
                                            h4("Bottom 5 Quantity by Country",
                                               style="text-align: center;"),
                                            plotOutput("multidecompositionPlot")),
                                        tabPanel(
                                            h4("Top 5 Quantity by Item Code",
                                               style="text-align: center;"), 
                                            plotOutput("acfPlot")),
                                        tabPanel(
                                            h4("Bottom 5 Quantity by Item Code",
                                               style="text-align: center;"), 
                                            plotOutput("acfPlot"))
                            )
                            
                            # Sales data
                        )
                    )
            )
        )
        )
    )
    
################
# Server
################
server <- function(input, output,session) {
    
}


shinyApp(ui, server)