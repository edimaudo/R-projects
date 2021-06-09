# Objective is to forecast sales and revenue

rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl','scales',
              'dplyr','mlbench','caTools','forecast','TTR','xts','lubridate','shiny','shinydashboard')
# load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

# load data
df <- read.csv("volume_by_day_type_and_region.csv")

#data update
df$date <- lubridate::as_date(df$date) #update date
df$revenue <- df$average_price*df$total_volume #revenue

# Drop downs
type_info <- sort(as.vector(unique(df$type))) #type 
type_info <- c(type_info,"All")
region_info <- sort(as.vector(unique(df$region))) #region
region_info <- c(region_info,"All")
forecast_info <- c(12,24) #forecast range

# Define UI for application
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
                  selectInput("typeInput", "Type", choices = type_info),
                  selectInput("regionInput", "Region", choices = region_info),
                  selectInput("forecastInput", "Forecast Period", choices = forecast_info),
                ),
                mainPanel(
                  h2("Forecast Analysis",style="text-align: center;"), 
                  fluidRow(
                    #h3("Amount pledged",style="text-align: center;"),
                    plotOutput("forecastOutput"),
                    #h3("# of pledges",style="text-align: center;"),
                    #plotOutput("pledgenumYearOutput")
                    #plotOutput(""),
                  )
                )
              )
      ) 
    )
  )
)


# Define server logic 
server <- function(input, output,session) {
  
  
}

shinyApp(ui, server)



