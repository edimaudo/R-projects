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
difference_info <- c("Yes","No")
log_info <- c("Yes","No")

#generate decomposition + plot

#ACT/PACF 

#Forecast with metric output


# visualization

# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "Patient Forecast"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Patient forecast", tabName = "forecast", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "forecast",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("aggregateInput", "Aggregate", choices = aggregate_info, selected = 'daily'),
                            selectInput("horizonInput", "Horizon", choices = horizon_info, selected = 14),
                            selectInput("frequencyInput", "Frequency", choices = frequency_info),
                            radioButtons("differenceInput","Difference",choices = difference_info, selected = "No"),
                            radioButtons("logInput","Log",choices = log_info, selected = "No"),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Forecast Analysis",style="text-align: center;"), 
                            fluidRow(
                                h3("Decomposition",style="text-align: center;"),
                                #plotOutput("pledgeYearOutput"),
                                h3("ACF/PACF",style="text-align: center;"),
                                #plotOutput("pledgenumYearOutput")
                                h3("Forecast",style="text-align: center;"),
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


