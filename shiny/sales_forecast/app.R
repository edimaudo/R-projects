# Objective is to forecast Revenue

rm(list = ls()) #clear environment

#===============
# libraries
#===============
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl','scales',
              'dplyr','mlbench','caTools','forecast','TTR','xts','lubridate','shiny',
              'shinydashboard')
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
frequency_info <- c(7, 12, 52, 365)
model_info <- c('auto arima','auto exponential','simple exponential',
                'double exponential','triple exponential')

#===============
# Define UI for application
#===============
ui <- dashboardPage(
  dashboardHeader(title = "Forecasting Analysis"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("sales forecast", tabName = "forecast", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "forecast",
              sidebarLayout(
                sidebarPanel(
                  selectInput("aggregateInput", "Aggregate", 
                              choices = aggregate_info, selected = 'weekly'),
                  selectInput("typeInput", "Type", 
                              choices = type_info,selected = "All"),
                  selectInput("regionInput", "Region", 
                              choices = region_info, selected = "All"),
                  selectInput("frequencyInput", "Frequency", 
                              choices = frequency_info, selected = 7),
                  selectInput("horizonInput", "Forecast Horizon", 
                              choices = horizon_info, selected = 12),
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
    
    # type ^ region info
    if (input$typeInput == 'All' && input$regionInput == "All"){
      df_output <- df %>%
        group_by(date) %>%
        summarise(Revenue = sum(revenue))    
    } else if (input$typeInput == 'All' && input$regionInput != "All") {
      df_output <- df %>%
        group_by(date) %>%
        filter(region  == input$regionInput) %>%
        summarise(Revenue = sum(revenue))     
    } else if (input$typeInput != 'All' && input$regionInput == "All") {
      df_output <- df %>%
        group_by(date) %>%
        filter(type == input$typeInput) %>%
        summarise(Revenue = sum(revenue))        
    } else {
      df_output <- df %>%
        group_by(date) %>%
        filter(type == input$typeInput,region  == input$regionInput) %>%
        summarise(Revenue = sum(revenue))        
    }
    
    
    #aggregation
    if(input$aggregateInput == 'weekly'){
      sales.xts <- xts(x = df_output$Revenue, order.by = df_output$date) 
      sales.data <- apply.weekly(sales.xts,mean)
      sales.end <- floor(0.8*length(sales.data)) #select the first 80% of the data
      sales.train <- sales.data[1:sales.end,] 
      sales.test <- sales.data[(sales.end+1):length(sales.data),]
      sales.start <- c(year (start(sales.train)), month(start(sales.train)),
                       week(start(sales.train)))
      sales.end <- c(year(end(sales.train)), month(end(sales.train)), week(end(sales.train)))
      sales.train <- ts(as.numeric(sales.train), start = sales.start, 
                        end = sales.end, frequency = as.numeric(input$frequencyInput) )
      sales.start <- c(year (start(sales.test)), month(start(sales.test)),
                       week(start(sales.test)))
      sales.end <- c(year(end(sales.test)), month(end(sales.test)), week(end(sales.test)))
      sales.test <- ts(as.numeric(sales.test), start = sales.start, 
                       end = sales.end, frequency = as.numeric(input$frequencyInput))    
    } 
    else {
      sales.xts <- xts(x = df$Revenue, order.by = df$date) 
      sales.data <- apply.monthly(sales.xts, mean) 
      sales.end <- floor(0.8*length(sales.data)) #select the first 80% of the data
      sales.train <- sales.data[1:sales.end,] 
      sales.test <- sales.data[(sales.end+1):length(sales.data),]
      sales.start <- c(year (start(sales.train)), month(start(sales.train)),
                       week(start(sales.train)))
      sales.end <- c(year(end(sales.train)), month(end(sales.train)), week(end(sales.train)))
      sales.train <- ts(as.numeric(sales.train), start = sales.start, 
                        end = sales.end, frequency = as.numeric(input$frequencyInput) )
      sales.start <- c(year (start(sales.test)), month(start(sales.test)),
                       week(start(sales.test)))
      sales.end <- c(year(end(sales.test)), month(end(sales.test)), week(end(sales.test)))
      sales.test <- ts(as.numeric(sales.test), start = sales.start, 
                       end = sales.end, frequency = as.numeric(input$frequencyInput))       
      
    }
    
    
    #set forecast horizon
    forecast.horizon <- as.numeric(input$horizonInput)
    
    # models
    if(input$modelInput == 'auto exponential'){
      sales.train%>% 
        ets %>% 
        forecast(h=forecast.horizon) %>% 
        plot()
      lines(sales.test, col = "red")             
    } else if (input$modelInput == 'auto arima'){
      sales.train%>% 
        auto.arima %>% 
        forecast(h=forecast.horizon) %>% 
        plot()
      lines(sales.test, col = "red")             
    } else if (input$modelInput == 'simple exponential'){
      sales.train%>% 
        HoltWinters(beta=FALSE, gamma=FALSE) %>% 
        forecast(h=forecast.horizon) %>% 
        plot()
      lines(sales.test, col = "red")             
    } else if (input$modelInput == 'double exponential'){
      sales.train%>% 
        HoltWinters(beta = TRUE, gamma=FALSE) %>% 
        forecast(h=forecast.horizon) %>% 
        plot()
      lines(sales.test, col = "red") 
    } else {
      sales.train%>% 
        HoltWinters(beta = TRUE, gamma = TRUE) %>% 
        forecast(h=forecast.horizon) %>% 
        plot()
      lines(sales.test, col = "red")           
    }
      
    
    
    
  })
  
  # accuracy output
  output$accuracyOutput <- DT::renderDataTable({
    # type ^ region info
    if (input$typeInput == 'All' && input$regionInput == "All"){
      df_output <- df %>%
        group_by(date) %>%
        summarise(Revenue = sum(revenue))    
    } else if (input$typeInput == 'All' && input$regionInput != "All") {
      df_output <- df %>%
        group_by(date) %>%
        filter(region  == input$regionInput) %>%
        summarise(Revenue = sum(revenue))     
    } else if (input$typeInput != 'All' && input$regionInput == "All") {
      df_output <- df %>%
        group_by(date) %>%
        filter(type == input$typeInput) %>%
        summarise(Revenue = sum(revenue))        
    } else {
      df_output <- df %>%
        group_by(date) %>%
        filter(type == input$typeInput,region  == input$regionInput) %>%
        summarise(Revenue = sum(revenue))        
    }
    
    
    #aggregation
    if(input$aggregateInput == 'weekly'){
      sales.xts <- xts(x = df_output$Revenue, order.by = df_output$date) 
      sales.data <- apply.weekly(sales.xts,mean)
      sales.end <- floor(0.8*length(sales.data)) #select the first 80% of the data
      sales.train <- sales.data[1:sales.end,] 
      sales.test <- sales.data[(sales.end+1):length(sales.data),]
      sales.start <- c(year (start(sales.train)), month(start(sales.train)),
                       week(start(sales.train)))
      sales.end <- c(year(end(sales.train)), month(end(sales.train)), week(end(sales.train)))
      sales.train <- ts(as.numeric(sales.train), start = sales.start, 
                        end = sales.end, frequency = as.numeric(input$frequencyInput) )
      sales.start <- c(year (start(sales.test)), month(start(sales.test)),
                       week(start(sales.test)))
      sales.end <- c(year(end(sales.test)), month(end(sales.test)), week(end(sales.test)))
      sales.test <- ts(as.numeric(sales.test), start = sales.start, 
                       end = sales.end, frequency = as.numeric(input$frequencyInput))    
    } 
    else {
      sales.xts <- xts(x = df$Revenue, order.by = df$date) 
      sales.data <- apply.monthly(sales.xts, mean) 
      sales.end <- floor(0.8*length(sales.data)) #select the first 80% of the data
      sales.train <- sales.data[1:sales.end,] 
      sales.test <- sales.data[(sales.end+1):length(sales.data),]
      sales.start <- c(year (start(sales.train)), month(start(sales.train)),
                       week(start(sales.train)))
      sales.end <- c(year(end(sales.train)), month(end(sales.train)), week(end(sales.train)))
      sales.train <- ts(as.numeric(sales.train), start = sales.start, 
                        end = sales.end, frequency = as.numeric(input$frequencyInput) )
      sales.start <- c(year (start(sales.test)), month(start(sales.test)),
                       week(start(sales.test)))
      sales.end <- c(year(end(sales.test)), month(end(sales.test)), week(end(sales.test)))
      sales.test <- ts(as.numeric(sales.test), start = sales.start, 
                       end = sales.end, frequency = as.numeric(input$frequencyInput))       
      
    }
    
    
    #set forecast horizon
    forecast.horizon <- as.numeric(input$horizonInput)
    
    # models
    if(input$modelInput == 'auto exponential'){
      patient.train.forecast <- ets(patient.train) %>% 
        forecast(h=forecast.horizon)    
    } else if (input$modelInput == 'auto arima'){
      patient.train.forecast <- auto.arima(patient.train) %>% 
        forecast(h=forecast.horizon)             
    } else if (input$modelInput == 'simple exponential'){
      patient.train.forecast <- HoltWinters(patient.train,
                                            beta=FALSE, 
                                            gamma=FALSE) %>% 
        forecast(h=forecast.horizon)             
    } else if (input$modelInput == 'double exponential'){
      patient.train.forecast <- HoltWinters(patient.train,
                                            beta=TRUE, 
                                            gamma=FALSE) %>% 
        forecast(h=forecast.horizon)  
    } else {
      patient.train.forecast <- HoltWinters(patient.train,
                                            beta=TRUE, 
                                            gamma=TRUE) %>% 
        forecast(h=forecast.horizon)  
    }
    outputInfo <- as.data.frame(accuracy(sales.train.forecast,sales.test))
    outputInfo    
    
    
  })
  
}

shinyApp(ui, server)



