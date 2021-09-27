#=============
# Business Forecast
#=============

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
# Load data
#=============
forecast_df <- read_excel("Forecast Data.xlsx",sheet="Data")
year_info <- read_excel("Forecast Data.xlsx",sheet="YearInfo")
week_info <- read_excel("Forecast Data.xlsx",sheet="WeekInfo")

#=============
# Data munging
#=============
forecast_aggregate <- forecast_df %>%
    inner_join(week_info,by = "WeekNo") %>%
    inner_join(year_info,by = "Year") %>%
    group_by(YearNo, Month2, Website) %>%
    dplyr::summarise(Turnover_total = sum(Turnover), 
                     Profit_total = sum(Profit), 
                     CustomerCount_total = sum(CustomerCount)) %>%
    select(YearNo, Month2, Website, Turnover_total, Profit_total, CustomerCount_total)

forecast_aggregate$dateInfo <- paste(forecast_aggregate$YearNo,forecast_aggregate$Month2,sep="-") 
forecast_aggregate$dateInfo2 <- as.Date(paste(forecast_aggregate$dateInfo,"-01",sep=""))

df <- forecast_aggregate %>%
    group_by(dateInfo2) %>%
    dplyr::summarise(Turnover = sum(Turnover_total), 
                     Profit = sum(Profit_total), 
                     CustomerCount = sum(CustomerCount_total)) %>%
    select(dateInfo2, Turnover, Profit, CustomerCount)

#=============
# Forecast Inputs
#=============
segment_info <- c("Profit","Turnover","Customer Count")
horizon_info <- c(1:50) #default 14
frequency_info <- c(7, 12, 52, 365)
difference_info <- c("Yes","No")
log_info <- c("Yes","No")
model_info <- c('auto-arima','auto-exponential','simple-exponential',
                'double-exponential','triple-exponential', 'tbat')
#=============
# Application UI
#=============
ui <- dashboardPage(
    dashboardHeader(title = "Business Forecast"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Analysis", tabName = "analysis", icon = icon("th")),
            menuItem("Forecasting", tabName = "Forecast", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "analysis",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("segmentInput", "Segment", 
                                        choices = segment_info, selected = "Profit"),
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
                            h1("Analysis",style="text-align: center;"), 
                            tabsetPanel(type = "tabs",
                                        tabPanel(
                                            h4("Decomposition",
                                               style="text-align: center;"),
                                            plotOutput("decompositionPlot")),
                                        tabPanel(
                                            h4("Multi seasonal Decomposition",
                                               style="text-align: center;"),
                                            plotOutput("multidecompositionPlot")),
                                        tabPanel(
                                            h4("ACF Plot",style="text-align: center;"), 
                                            plotOutput("acfPlot")),
                                        tabPanel(
                                            h4("PACF Plot",style="text-align: center;"), 
                                            plotOutput("pacfPlot"))
                            )
                        )
                    )  
            ),
            tabItem(tabName = "Forecast",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("segmentInput", "Segment", 
                                        choices = segment_info, selected = "Profit"),
                            selectInput("horizonInput", "Horizon", 
                                        choices = horizon_info, selected = 14),
                            selectInput("frequencyInput", "Frequency", 
                                        choices = frequency_info, selected = 7),
                            sliderInput("traintestInput", "Train/Test Split",
                                        min = 0, max = 1,value = 0.8),
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
# Server logic 
#=============
server <- function(input, output,session) {

    
    
    # decomoposition plot
    output$decompositionPlot <- renderPlot({
        
        if (input$segmentInput == 'Profit'){
            business.xts <- xts(x = df$Profit, order.by = df$dateInfo2) 
        } else if (input$segmentInput = 'Turnover'){
            business.xts <- xts(x = df$Turnover, order.by = df$dateInfo2) 
        } else {
            business.xts <- xts(x = df$CustomerCount, order.by = df$dateInfo2) 
        }
        
        business.monthly <- apply.monthly(business.xts, mean) 
        business.end <- floor(1*length(business.monthly)) 
        business.data <- business.monthly[1:business.end,] 
        business.start <- c(year(start(business.data)), month(start(business.data)))
        business.end <- c(year(end(business.data)), month(end(business.data)))
        business.data <- ts(as.numeric(business.data), start = business.start, 
                            end = business.end, frequency = as.numeric(input$frequencyInput)) 
        
        
    })
    
    # multi season output
    output$multidecompositionPlot <- renderPlot({
        
    })
    
    # ACF output
    output$acfPlot <- renderPlot({
        
    })
    
    # PACF output
    output$pacfPlot <- renderPlot({
        
    })
    
    # Forecast Visualization
    output$forecastPlot <- renderPlot({
        
    })
    
    
    # Forecast Results/Output
    output$forecastOutput <- DT::renderDataTable({
        
    })
    
    # Forecast Accuracy
    output$accuracyOutput <- DT::renderDataTable({
        
    })
}

shinyApp(ui, server)