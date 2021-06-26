# Patient Forecast
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl',
              'shiny','shinydashboard','scales','dplyr','mlbench','caTools',
              'forecast','TTR','xts','lubridate')

# load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#=============
# load data
#=============
mtry <- try(read.table("data.csv", sep = ",", header = TRUE), silent = TRUE)
if (class(mtry) != "try-error") {
    df <- read.csv("data.csv", sep = ",", header = TRUE)
    df[df==0] <- NA #assigne 0 to NA
    df <- na.omit(df) #remove na
    df$Arrival_date <- lubridate::dmy(df$Arrival_date) #update date field
} 
#else {
#    #message("File doesn't exist, please check")
#}

#=============
#dropdowns
#=============
aggregate_info <- c("daily",'weekly','monthly')
horizon_info <- c(1:50) #default 14
frequency_info <- c(7, 12, 52, 365)
difference_info <- c("Yes","No")
log_info <- c("Yes","No")
model_info <- c('auto arima','auto exponential','simple exponential',
                'double exponential','triple exponential', 'tbat','manual arima')


# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "Patient Forecast"),
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
                            fileInput("file1", "Choose CSV File", accept = ".csv"),
                            checkboxInput("header", "Header", TRUE)
                        ),
                        mainPanel(
                            tableOutput("contents")
                        )
                    )
            ),
            tabItem(tabName = "analysis",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("aggregateInput", "Aggregate", 
                                        choices = aggregate_info, selected = 'daily'),
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
                            selectInput("aggregateInput", "Aggregate", 
                                        choices = aggregate_info, selected = 'daily'),
                            selectInput("horizonInput", "Horizon", 
                                        choices = horizon_info, selected = 14),
                            selectInput("frequencyInput", "Frequency", 
                                        choices = frequency_info, selected = 7),
                            sliderInput("traintestInput", "Train/Test Split",
                                        min = 0, max = 1,value = 0.8),
                            checkboxGroupInput("modelInput", "Models",choices = model_info, 
                                               selected = 'auto exponential'),
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
                                        tabPanel(h4("Forecast Output",style="text-align: center;"), 
                                                 plotOutput("forecastPlot"))#,
                                        #tabPanel(h4("Forecast Accuracy",style="text-align: center;"), 
                                        #         DT::dataTableOutput("accuracyOutput"))
                            )
                        )
                    )
            ) 
        )
    )
)


# Define server logic 
server <- function(input, output,session) {
    
    # load data
    output$contents <- renderTable({
        file <- input$file1
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        df <- read.csv(file$datapath, header = input$header)
        df
        
        write_csv(df, "data.csv")
        
    })
    
    # decomposition output
    output$decompositionPlot <- renderPlot({
        patient.xts <- xts(x = df$Patients, order.by = df$Arrival_date) 
        patient.daily <- apply.daily(patient.xts,mean)
        patient.weekly <- apply.weekly(patient.xts, mean) 
        patient.monthly <- apply.monthly(patient.xts, mean) 
        #using only daily data
        if (input$aggregateInput == 'daily'){
            patient.end <- floor(1*length(patient.daily)) 
            patient.data <- patient.daily[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)),
                               day(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)), 
                             day(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))             
        } else if(input$aggregateInput == 'weekly'){
            patient.end <- floor(1*length(patient.weekly)) 
            patient.data <- patient.weekly[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)),
                               week(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)), 
                             week(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))         
        } else {
            patient.end <- floor(1*length(patient.monthly)) 
            patient.data <- ppatient.monthly[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))               
        }
        
        if (input$differenceInput == "Yes"){
            patient.data <- diff(patient.data, differences = as.numeric(input$differenceNumericInput)) 
        }
        #Decompose the Time Series
        patient.data %>%
            decompose() %>%
            autoplot()
        
    })
    
    # multi season output
    output$multidecompositionPlot <- renderPlot({
        patient.xts <- xts(x = df$Patients, order.by = df$Arrival_date) 
        patient.daily <- apply.daily(patient.xts,mean)
        patient.weekly <- apply.weekly(patient.xts, mean) 
        patient.monthly <- apply.monthly(patient.xts, mean) 
        #using only daily data
        if (input$aggregateInput == 'daily'){
            patient.end <- floor(1*length(patient.daily)) 
            patient.data <- patient.daily[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)),
                               day(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)), 
                             day(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))             
        } else if(input$aggregateInput == 'weekly'){
            patient.end <- floor(1*length(patient.weekly)) 
            patient.data <- patient.weekly[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)),
                               week(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)), 
                             week(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))         
        } else {
            patient.end <- floor(1*length(patient.monthly)) 
            patient.data <- ppatient.monthly[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))               
        }
        
        if (input$differenceInput == "Yes"){
            patient.data <- diff(patient.data, differences = as.numeric(input$differenceNumericInput))
        }
        #Decompose the Time Series
        patient.data %>%
            mstl() %>%
            autoplot()
        
    })    
    
    # ACF output
    output$acfPlot <- renderPlot({
        patient.xts <- xts(x = df$Patients, order.by = df$Arrival_date) 
        patient.daily <- apply.daily(patient.xts,mean)
        patient.weekly <- apply.weekly(patient.xts, mean) 
        patient.monthly <- apply.monthly(patient.xts, mean) 
        #using only daily data
        if (input$aggregateInput == 'daily'){
            patient.end <- floor(1*length(patient.daily)) 
            patient.data <- patient.daily[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)),
                               day(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)), 
                             day(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))             
        } else if(input$aggregateInput == 'weekly'){
            patient.end <- floor(1*length(patient.weekly)) 
            patient.data <- patient.weekly[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)),
                               week(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)), 
                             week(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))         
        } else {
            patient.end <- floor(1*length(patient.monthly)) 
            patient.data <- ppatient.monthly[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))               
        }
        
        if (input$logInput == "No"){
            #acf(patient.train)
            ggAcf(patient.data)
        } else {
            ggAcf(log(patient.data))
        }
        
        
        
    })
    
    # PACF output
    output$pacfPlot <- renderPlot({
        patient.xts <- xts(x = df$Patients, order.by = df$Arrival_date) 
        patient.daily <- apply.daily(patient.xts,mean)
        patient.weekly <- apply.weekly(patient.xts, mean) 
        patient.monthly <- apply.monthly(patient.xts, mean) 
        #using only daily data
        if (input$aggregateInput == 'daily'){
            patient.end <- floor(1*length(patient.daily)) 
            patient.data <- patient.daily[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)),
                               day(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)), 
                             day(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))             
        } else if(input$aggregateInput == 'weekly'){
            patient.end <- floor(1*length(patient.weekly)) 
            patient.data <- patient.weekly[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)),
                               week(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)), 
                             week(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))         
        } else {
            patient.end <- floor(1*length(patient.monthly)) 
            patient.data <- ppatient.monthly[1:patient.end,] 
            patient.start <- c(year (start(patient.data)), month(start(patient.data)))
            patient.end <- c(year(end(patient.data)), month(end(patient.data)))
            patient.data <- ts(as.numeric(patient.data), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))               
        }
        
        if (input$logInput == "No"){
            ggPacf(patient.data) #pacf(patient.train)
        } else {
            ggPacf(log(patient.data))
        }
        
    })
    
    # Forecast
    output$forecastPlot <- renderPlot({
        # Aggregation &  training and test data
        patient.xts <- xts(x = df$Patients, order.by = df$Arrival_date) 
        patient.daily <- apply.daily(patient.xts,mean)
        patient.weekly <- apply.weekly(patient.xts, mean) 
        patient.monthly <- apply.monthly(patient.xts, mean)        
        if(input$aggregateInput == 'daily'){
            patient.data <- apply.daily(patient.xts,mean)
            patient.end <- floor(as.numeric(input$traintestInput)  * length(patient.data)) 
            patient.train <- patient.data[1:patient.end,] 
            patient.test <- patient.data[(patient.end+1):length(patient.data),]
            patient.start <- c(year (start(patient.train)), month(start(patient.train)),
                               day(start(patient.train)))
            patient.end <- c(year(end(patient.train)), month(end(patient.train)), 
                             day(end(patient.train)))
            patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                                end = patient.end, frequency = as.numeric(input$frequencyInput) )
            patient.start <- c(year (start(patient.test)), month(start(patient.test)),
                               day(start(patient.test)))
            patient.end <- c(year(end(patient.test)), month(end(patient.test)), 
                             day(end(patient.test)))
            patient.test <- ts(as.numeric(patient.test), start = patient.start, 
                               end = patient.end, frequency = as.numeric(input$frequencyInput))
        } else if(input$aggregateInput == 'weekly'){
            patient.data <- apply.weekly(patient.xts, mean) 
            patient.end <- floor(as.numeric(input$traintestInput)*length(patient.data)) 
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
            patient.end <- floor(as.numeric(input$traintestInput)*length(patient.data)) 
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
        
        #manual arima info
        orderinfo = c(as.numeric(input$autoInput),as.numeric(input$difference2Input),
                       as.numeric(input$maInput))
        #models
        auto_exp_model <- patient.train %>% ets %>% forecast(h=forecast.horizon)
        auto_arima_model <- patient.train %>% auto.arima() %>% forecast(h=forecast.horizon)
        simple_exp_model <- patient.train %>% HoltWinters(beta=FALSE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        double_exp_model <- patient.train %>% HoltWinters(beta = TRUE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        triple_exp_model <- patient.train %>% HoltWinters(beta = TRUE, gamma = TRUE) %>% 
            forecast(h=forecast.horizon)
        tbat_model <- patient.train %>% tbats %>% forecast(h=forecast.horizon)
        manual_arima_model <- patient.train %>% arima(order=orderinfo) 
        
        #model output
        auto_arima <- "auto arima"        %in% input$modelInput
        auto_exp   <- 'auto exponential'  %in% input$modelInput
        simple_exp <- "simple exponential"%in% input$modelInput
        double_exp <- "double exponential"%in% input$modelInput
        triple_exp <- "triple exponential"%in% input$modelInput
        tbat       <- "tbat"  %in% input$modelInput
        manual_arima <- "manual arima"  %in% input$modelInput
        auto_arima_exp <- c("auto arima","auto exponential") %in% input$modelInput
        
        model_selection <- c(input$modelInput)     
        
          
        if (is.null(input$modelInput)){
             
        } else if (auto_arima ){
            auto_arima_model %>% autoplot()
        }  else if (auto_exp) {
            auto_exp_model %>% autoplot()
        }  else if (simple_exp) {
            simple_exp_model %>% autoplot()
        }  else if (double_exp) {
            double_exp_model %>% autoplot()
        }else if (triple_exp) {
            triple_exp_model %>% autoplot()
        } else if (tbat ) {
            tbat_model %>% autoplot()
        } else if (manual_arima){
            manual_arima_model %>% autoplot()
        } else if (""){
            autoplot(patient.train) + autolayer(auto_arima_model,series="auto arima")
            + autolayer(auto_exp_model, series = "auto exponential")
        }
            
    })
    
    # Forecast output
    output$accuracyOutput <- DT::renderDataTable({
        patient.xts <- xts(x = df$Patients, order.by = df$Arrival_date) 
        patient.daily <- apply.daily(patient.xts,mean)
        patient.weekly <- apply.weekly(patient.xts, mean) 
        patient.monthly <- apply.monthly(patient.xts, mean) 
        # Aggregation &  training and test data
        if(input$aggregateInput == 'daily'){
            patient.data <- apply.daily(patient.xts,mean)
            patient.end <- floor(as.numeric(input$traintestInput)*length(patient.data)) 
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
            patient.end <- floor(as.numeric(input$traintestInput)*length(patient.data)) 
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
            patient.end <- floor(as.numeric(input$traintestInput)*length(patient.data)) 
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
        
        outputInfo <- as.data.frame(accuracy(patient.train.forecast ,patient.test))
        
        DT::datatable(outputInfo, options = list(scrollX = TRUE))
        
    })
    
}

shinyApp(ui, server)


