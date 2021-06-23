# Patient Forecast
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl','shiny','shinydashboard',
              'scales','dplyr','mlbench','caTools','forecast','TTR','xts',
              'lubridate')
# load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#=============
#load data
#=============
df <- read.csv("test-ts2.csv")

#=============
#data cleaning
#=============
df[df==0] <- NA #assigne 0 to NA
df <- na.omit(df) #remove na
df$Arrival_date <- lubridate::dmy(df$Arrival_date) #update date field

#=============
#dropdowns
#=============
aggregate_info <- c("daily",'weekly','monthly')
horizon_info <- c(1:50) #default 14
frequency_info <- c(7, 12, 52, 365)
difference_info <- c("Yes","No")
log_info <- c("Yes","No")
model_info <- c('auto arima','auto exponential','simple exponential',
                'double exponential','triple exponential')

#=============
# data
#=============
patient.xts <- xts(x = df$Patients, order.by = df$Arrival_date) 
patient.daily <- apply.daily(patient.xts,mean)
patient.weekly <- apply.weekly(patient.xts, mean) 
patient.monthly <- apply.monthly(patient.xts, mean) 

# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "Patient Forecast"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Patient Forecast", tabName = "Forecast", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Forecast",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("aggregateInput", "Aggregate", 
                                        choices = aggregate_info, selected = 'daily'),
                            selectInput("horizonInput", "Horizon", 
                                        choices = horizon_info, selected = 14),
                            selectInput("frequencyInput", "Frequency", 
                                        choices = frequency_info, selected = 7),
                            radioButtons("differenceInput","Difference",
                                         choices = difference_info, selected = "No"),
                            radioButtons("logInput","Log",
                                         choices = log_info, selected = "No"),
                            selectInput("modelInput", "Model", 
                                        choices = model_info, selected = 'auto exponential'),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Forecast Analysis",style="text-align: center;"), 
                            tabsetPanel(type = "tabs",
                                        tabPanel(h4("Decomposition",style="text-align: center;"), 
                                                 plotOutput("decompositionPlot")),
                                        tabPanel(h4("ACF Plot",style="text-align: center;"), 
                                                 plotOutput("acfPlot")),
                                        tabPanel(h4("PACF Plot",style="text-align: center;"), 
                                                 plotOutput("pacfPlot")),
                                        tabPanel(h4("Forecast Output",style="text-align: center;"), 
                                                 plotOutput("forecastPlot")),
                                        tabPanel(h4("Forecast Accuracy",style="text-align: center;"), 
                                                 DT::dataTableOutput("accuracyOutput"))
                            )
                        )
                    )
            ) 
        )
    )
)


# Define server logic 
server <- function(input, output,session) {
    
    #decomposition output
    output$decompositionPlot <- renderPlot({
        #using only daily data
        patient.end <- floor(1*length(patient.daily)) 
        patient.train <- patient.daily[1:patient.end,] 
        patient.start <- c(year (start(patient.train)), month(start(patient.train)),
                           day(start(patient.train)))
        patient.end <- c(year(end(patient.train)), month(end(patient.train)), 
                         day(end(patient.train)))
        patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                             end = patient.end, frequency = as.numeric(input$frequencyInput)) 
         
         if (input$differenceInput == "Yes"){
             patient.train <- diff(patient.train, differences = 1) # hardcoded difference value
         }
        #Decompose the Time Series
        patient.train %>%
            decompose() %>%
            autoplot()
        
    })
    
    #ACF output
    output$acfPlot <- renderPlot({
        #using only daily data
        patient.end <- floor(1*length(patient.daily))
        patient.train <- patient.daily[1:patient.end,] 
        patient.start <- c(year (start(patient.train)), month(start(patient.train)),
                           day(start(patient.train)))
        patient.end <- c(year(end(patient.train)), month(end(patient.train)), day(end(patient.train)))
        patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                            end = patient.end, frequency = as.numeric(input$frequencyInput)) 
        
        if (input$logInput == "No"){
            #acf(patient.train)
            ggAcf(patient.train)
        } else {
            ggAcf(log(patient.train))
        }
        
        
        
    })

    #PACF output
    output$pacfPlot <- renderPlot({
        #using only daily data
        patient.end <- floor(1*length(patient.daily)) #select the first 80% of the data
        patient.train <- patient.daily[1:patient.end,] 
        patient.start <- c(year (start(patient.train)), month(start(patient.train)),
                           day(start(patient.train)))
        patient.end <- c(year(end(patient.train)), month(end(patient.train)), day(end(patient.train)))
        patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                            end = patient.end, frequency = as.numeric(input$frequencyInput)) 
        
        if (input$logInput == "No"){
            ggPacf(patient.train) #pacf(patient.train)
        } else {
            ggPacf(log(patient.train))
        }
        
    })
    
    output$forecastPlot <- renderPlot({
        # Aggregation &  training and test data
        if(input$aggregateInput == 'daily'){
            patient.data <- apply.daily(patient.xts,mean)
            patient.end <- floor(0.8*length(patient.data)) #select the first 80% of the data
            patient.train <- patient.data[1:patient.end,] 
            patient.test <- patient.data[(patient.end+1):length(patient.data),]
            patient.start <- c(year (start(patient.train)), month(start(patient.train)),
                               day(start(patient.train)))
            patient.end <- c(year(end(patient.train)), month(end(patient.train)), day(end(patient.train)))
            patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                                end = patient.end, frequency = as.numeric(input$frequencyInput) )
            patient.start <- c(year (start(patient.test)), month(start(patient.test)),
                               day(start(patient.test)))
            patient.end <- c(year(end(patient.test)), month(end(patient.test)), day(end(patient.test)))
            patient.test <- ts(as.numeric(patient.test), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))
        } else if(input$aggregateInput == 'weekly'){
            patient.data <- apply.weekly(patient.xts, mean) 
            patient.end <- floor(0.8*length(patient.data)) #select the first 80% of the data
            patient.train <- patient.data[1:patient.end,] 
            patient.test <- patient.data[(patient.end+1):length(patient.data),]
            patient.start <- c(year (start(patient.train)), month(start(patient.train)),
                               week(start(patient.train)))
            patient.end <- c(year(end(patient.train)), month(end(patient.train)), 
                             week(end(patient.train)))
            patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                                end = patient.end, frequency = as.numeric(input$frequencyInput) )
            patient.start <- c(year (start(patient.test)), month(start(patient.test)),
                               week(start(patient.test)))
            patient.end <- c(year(end(patient.test)), month(end(patient.test)), 
                             week(end(patient.test)))
            patient.test <- ts(as.numeric(patient.test), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))
        } else {
            patient.data <- apply.monthly(patient.xts, mean) 
            patient.data <- apply.weekly(patient.xts, mean) 
            patient.end <- floor(0.8*length(patient.data)) #select the first 80% of the data
            patient.train <- patient.data[1:patient.end,] 
            patient.test <- patient.data[(patient.end+1):length(patient.data),]
            patient.start <- c(year (start(patient.train)), month(start(patient.train)))
            patient.end <- c(year(end(patient.train)), month(end(patient.train)))
            patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                                end = patient.end, frequency = as.numeric(input$frequencyInput) )
            patient.start <- c(year (start(patient.test)), month(start(patient.test)))
            patient.end <- c(year(end(patient.test)), month(end(patient.test)))
            patient.test <- ts(as.numeric(patient.test), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))
        }
        
        #set forecast horizon
        forecast.horizon <- as.numeric(input$horizonInput)
 
        # models
        if(input$modelInput == 'auto exponential'){
            patient.train%>% 
                ets %>% 
                forecast(h=forecast.horizon) %>% 
                autoplot()
            #lines(patient.test, col = "red")             
        } else if (input$modelInput == 'auto arima'){
            patient.train%>% 
                auto.arima %>% 
                forecast(h=forecast.horizon) %>% 
                autoplot()
            #lines(patient.test, col = "red")             
        } else if (input$modelInput == 'simple exponential'){
            patient.train%>% 
                HoltWinters(beta=FALSE, gamma=FALSE) %>% 
                forecast(h=forecast.horizon) %>% 
                
                autoplot()
           # lines(patient.test, col = "red")             
        } else if (input$modelInput == 'double exponential'){
            patient.train%>% 
                HoltWinters(beta = TRUE, gamma=FALSE) %>% 
                forecast(h=forecast.horizon) %>% 
                autoplot()
            #lines(patient.test, col = "red") 
        } else {
            patient.train%>% 
                HoltWinters(beta = TRUE, gamma = TRUE) %>% 
                forecast(h=forecast.horizon) %>% 
                autoplot()
            #lines(patient.test, col = "red") 
            
        }
    })
    
    
    output$accuracyOutput <- DT::renderDataTable({
        # Aggregation &  training and test data
        if(input$aggregateInput == 'daily'){
            patient.data <- apply.daily(patient.xts,mean)
            patient.end <- floor(0.8*length(patient.data)) #select the first 80% of the data
            patient.train <- patient.data[1:patient.end,] 
            patient.test <- patient.data[(patient.end+1):length(patient.data),]
            patient.start <- c(year (start(patient.train)), month(start(patient.train)),
                               day(start(patient.train)))
            patient.end <- c(year(end(patient.train)), month(end(patient.train)), day(end(patient.train)))
            patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                                end = patient.end, frequency = as.numeric(input$frequencyInput) )
            patient.start <- c(year (start(patient.test)), month(start(patient.test)),
                               day(start(patient.test)))
            patient.end <- c(year(end(patient.test)), month(end(patient.test)), day(end(patient.test)))
            patient.test <- ts(as.numeric(patient.test), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))
        } else if(input$aggregateInput == 'weekly'){
            patient.data <- apply.weekly(patient.xts, mean) 
            patient.end <- floor(0.8*length(patient.data)) #select the first 80% of the data
            patient.train <- patient.data[1:patient.end,] 
            patient.test <- patient.data[(patient.end+1):length(patient.data),]
            patient.start <- c(year (start(patient.train)), month(start(patient.train)),
                               week(start(patient.train)))
            patient.end <- c(year(end(patient.train)), month(end(patient.train)), 
                             week(end(patient.train)))
            patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                                end = patient.end, frequency = as.numeric(input$frequencyInput) )
            patient.start <- c(year (start(patient.test)), month(start(patient.test)),
                               week(start(patient.test)))
            patient.end <- c(year(end(patient.test)), month(end(patient.test)), 
                             week(end(patient.test)))
            patient.test <- ts(as.numeric(patient.test), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))
        } else {
            patient.data <- apply.monthly(patient.xts, mean) 
            patient.data <- apply.weekly(patient.xts, mean) 
            patient.end <- floor(0.8*length(patient.data)) #select the first 80% of the data
            patient.train <- patient.data[1:patient.end,] 
            patient.test <- patient.data[(patient.end+1):length(patient.data),]
            patient.start <- c(year (start(patient.train)), month(start(patient.train)))
            patient.end <- c(year(end(patient.train)), month(end(patient.train)))
            patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                                end = patient.end, frequency = as.numeric(input$frequencyInput) )
            patient.start <- c(year (start(patient.test)), month(start(patient.test)))
            patient.end <- c(year(end(patient.test)), month(end(patient.test)))
            patient.test <- ts(as.numeric(patient.test), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))
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
        
        outputInfo <- as.data.frame(accuracy(patient.train.forecast,patient.test))
        
        DT::datatable(outputInfo, options = list(scrollX = TRUE))
        
    })
    
   

 
    
}

shinyApp(ui, server)


