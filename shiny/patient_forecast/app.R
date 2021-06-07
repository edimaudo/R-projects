# Patient Forecast
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl',
              'scales','dplyr','mlbench','caTools','forecast','TTR','xts',
              'lubridate')
# load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#load data
df <- read.csv("test-ts2.csv")

#data cleaning
df <- na.omit(df)
df$Arrival_date <- lubridate::dmy(df$Arrival_date) #update date field

#dropdowns
aggregate_info <- c("daily",'weekly','monthly')
horizon_info <- c(1:50) #default 14
frequency_info <- c(7, 12, 52.18, 365.25)

#generate decomposition + plot

#ACT/PACF 

#Forecast with metric output


# visualization

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
                                #plotOutput("pledgeYearOutput"),
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


