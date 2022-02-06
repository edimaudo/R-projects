#############################
# MET Police Crime Forecast
#############################

rm(list = ls()) #clear environment
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse','readxl',
              'shiny','shinydashboard','scales','dplyr','mlbench','caTools',
              'forecast','TTR','xts','lubridate')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#=============
# Data
#=============
offences_past <- read_excel("Met Police Offence Data.xlsx",sheet="Offences Apr 2010 - Sep 2019")
offences_future <- read_excel("Met Police Offence Data.xlsx",sheet="Offences Oct 2019 - Mar 2020")

crime_columns <- c( 'Month','Burglary - Business and Community','Burglary - Residential','Other Sexual Offences',
                    'Rape',
                    'Bicycle Theft','Other Theft',	'Shoplifting',	'Theft from Person',
                    'Homicide',	'Violence with Injury',	'Violence without Injury')

offences_past <- offences_past %>%
    select(crime_columns)

offences_future <- offences_future %>%
    select(crime_columns)

#=============
# Dropdowns
#=============
crime_info <- crime_columns[-c(1)]
aggregate_info <- c('weekly','monthly')
horizon_info <- c(1:50) 
frequency_info <- c(7, 12, 52, 365)
difference_info <- c("Yes","No")
log_info <- c("Yes","No")
model_info <- c('auto-arima','auto-exponential','simple-exponential',
                'double-exponential','triple-exponential', 'tbat')
#other models ETS, ARIMA, STL-ETS, NNAR
#https://otexts.com/fpp2/combinations.html

#=============
# Define UI for application
#=============
ui <- dashboardPage(
    dashboardHeader(title = "Met Crime Forecaster"),
    dashboardSidebar(
        sidebarMenu(
            #menuItem("Introduction",tabName = "intro",icon=icon("th")),
            menuItem("Analysis", tabName = "analysis", icon = icon("th")),
            menuItem("Forecasting", tabName = "forecast", icon = icon("th"))
        ) 
    ),
    dashboardBody(
        tabItems(
            #tabItem(tabName = "intro",includeMarkdown("about.md"),hr()),
            #----------
            # Analysis
            #----------
            tabItem(tabName = "analysis",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput('crimeTypeInput',"Crime Type",choices=crime_info,
                                        selected = 'Homicide'),
                            selectInput("aggregateInput", "Aggregate", 
                                        choices = aggregate_info, selected = 'weekly'),
                            selectInput("frequencyInput", "Frequency", 
                                        choices = frequency_info, selected = 7),
                            radioButtons("differenceInput","Difference",
                                         choices = difference_info, selected = "No"),
                            numericInput("differenceNumericInput", "Difference Input", 
                                         1, min = 1, max = 52, step = 0.5),
                            radioButtons("logInput","Log",
                                         choices = log_info, selected = "No"),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            #h1("Analysis",style="text-align: center;"), 
                            DT::dataTableOutput("test")
                            # tabsetPanel(type = "tabs",
                            #             tabPanel(
                            #                 h4("Trend Visualization",
                            #                    style="text-align: center;"), 
                            #                     plotOutput("trendPlot")),
                            #             tabPanel(
                            #                 h4("Decomposition",
                            #                    style="text-align: center;"),
                            #                 plotOutput("decompositionPlot")),
                            #             tabPanel(
                            #                 h4("Multi seasonal Decomposition",
                            #                    style="text-align: center;"),
                            #                 plotOutput("multidecompositionPlot")),
                            #             tabPanel(
                            #                 h4("ACF Plot",style="text-align: center;"), 
                            #                 plotOutput("acfPlot")),
                            #             tabPanel(
                            #                 h4("PACF Plot",style="text-align: center;"), 
                            #                 plotOutput("pacfPlot"))
                            # )
                        )
                    )  
            ),
            #-----------
            # Forecast
            #-----------
            tabItem(tabName = "forecast",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("aggregateInput", "Aggregate", 
                                        choices = aggregate_info, selected = 'weekly'),
                            selectInput("horizonInput", "Horizon", 
                                        choices = horizon_info, selected = 14),
                            selectInput("frequencyInput", "Frequency", 
                                        choices = frequency_info, selected = 7),
                            sliderInput("traintestInput", "Train/Test Split",
                                        min = 0, max = 1,value = 0.8),
                            checkboxGroupInput("modelInput", "Models",choices = model_info, 
                                               selected = model_info),
                            sliderInput("autoInput", "Auto-regression",
                                        min = 0, max = 100,value = 0),
                            sliderInput("difference2Input", "Difference",
                                        min = 0, max = 52,value = 0),
                            sliderInput("maInput", "Moving Average",
                                        min = 0, max = 100,value = 0),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Forecast Analysis",style="text-align: center;"), 
                            tabsetPanel(type = "tabs",
                                        tabPanel(h4("Forecast Visualization",style="text-align: center;"), 
                                                 plotOutput("forecastPlot")),
                                        tabPanel(h4("Forecast Results",style="text-align: center;"), 
                                                 DT::dataTableOutput("forecastOutput")),
                                        tabPanel(h4("Forecast Accuracy",style="text-align: center;"), 
                                                 DT::dataTableOutput("accuracyOutput"))
                            )
                        )
                    )
            ) 
        )
    ) 
)   


#=============
# Define server logic 
#=============
server <- function(input, output,session) {
    
    #----------
    # Analysis
    #----------
    output$test <- DT::renderDataTable({
        column_info <- colnames(offences_past)
        columndata <- column_info[column_info %in% c('Month',input$crimeTypeInput)]
        output_df <- offences_past %>%
            select(columndata)
            #select(!!!input$crimeTypeInput, Month)
        DT::datatable(output_df)
    })
    
    output$trendPlot <- renderPlot({
        
    })
    
    output$decompositionPlot <- renderPlot({
        
    })
    
    output$multidecompositionPlot <- renderPlot({
        
    })
    
    output$pacfPlot <- renderPlot({
        
    })
    
    #----------
    # Forecast
    #----------
}


# Run the application 
shinyApp(ui = ui, server = server)