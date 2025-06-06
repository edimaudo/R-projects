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
                            h1("Forecasting",style="text-align: center;"), 
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

    #==================
    # decomoposition plot
    #==================
    output$decompositionPlot <- renderPlot({
        
        if (input$segmentInput == 'Profit'){
            business.xts <- xts(x = df$Profit, order.by = df$dateInfo2) 
        } else if (input$segmentInput == 'Turnover'){
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
        
        
        if (input$differenceInput == "Yes"){
            business.data <- diff(business.data, differences = as.numeric(input$differenceNumericInput)) 
        }
        #Decompose the Time Series
        business.data %>%
            decompose() %>%
            autoplot()
        
    })
    #==================
    # multi season output
    #==================
    output$multidecompositionPlot <- renderPlot({
        if (input$segmentInput == 'Profit'){
            business.xts <- xts(x = df$Profit, order.by = df$dateInfo2) 
        } else if (input$segmentInput == 'Turnover'){
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
        
        
        if (input$differenceInput == "Yes"){
            business.data <- diff(business.data, differences = as.numeric(input$differenceNumericInput)) 
        }
        #Decompose the Time Series
        business.data %>%
            mstl() %>%
            autoplot() 
    })
    #==================
    # ACF output
    #==================
    output$acfPlot <- renderPlot({
        if (input$segmentInput == 'Profit'){
            business.xts <- xts(x = df$Profit, order.by = df$dateInfo2) 
        } else if (input$segmentInput == 'Turnover'){
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
        
        if (input$logInput == "No"){
            ggAcf(business.data)
        } else {
            ggAcf(log(business.data))
        }
    })
    #==================
    # PACF output
    #==================
    output$pacfPlot <- renderPlot({
        if (input$segmentInput == 'Profit'){
            business.xts <- xts(x = df$Profit, order.by = df$dateInfo2) 
        } else if (input$segmentInput == 'Turnover'){
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
        
        if (input$logInput == "No"){
            ggPacf(business.data)
        } else {
            ggPacf(log(business.data))
        }
    })
    
    #==================
    # Forecast Visualization
    #==================
    output$forecastPlot <- renderPlot({
       
        if (input$segmentInput == 'Profit'){
            business.xts <- xts(x = df$Profit, order.by = df$dateInfo2) 
        } else if (input$segmentInput == 'Turnover'){
            business.xts <- xts(x = df$Turnover, order.by = df$dateInfo2) 
        } else {
            business.xts <- xts(x = df$CustomerCount, order.by = df$dateInfo2) 
        }
        
        business.data <- apply.monthly(business.xts, mean) 
        business.end <- floor(as.numeric(input$traintestInput)*length(business.data)) 
        business.train <- business.data[1:business.end,] 
        business.test <- business.data[(business.end+1):length(business.data),]
        business.start <- c(year (start(business.train)), month(start(business.train)))
        business.end <- c(year(end(business.train)), month(end(business.train)))
        business.train <- ts(as.numeric(business.train), start = business.start, 
                             end = business.end, frequency = as.numeric(input$frequencyInput) )
        business.start <- c(year (start(business.test)), month(start(business.test)))
        business.end <- c(year(end(business.test)), month(end(business.test)))
        business.test <- ts(as.numeric(business.test), start = business.start, 
                            end = business.end, frequency = as.numeric(input$frequencyInput))
        
        # set forecast horizon
        forecast.horizon <- as.numeric(input$horizonInput)
        
        # models
        auto_exp_model <- business.train %>% ets %>% forecast(h=forecast.horizon)
        auto_arima_model <- business.train %>% auto.arima() %>% forecast(h=forecast.horizon)
        simple_exp_model <- business.train %>% HoltWinters(beta=FALSE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        double_exp_model <- business.train %>% HoltWinters(beta = TRUE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        triple_exp_model <- business.train %>% HoltWinters(beta = TRUE, gamma = TRUE) %>% 
            forecast(h=forecast.horizon)
        tbat_model <- business.train %>% tbats %>% forecast(h=forecast.horizon)
        
        autoplot(business.train) +
            autolayer(auto_arima_model,series="auto arima", alpha=0.2) +
            autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
            autolayer(simple_exp_model, series= "simple exponential", alpha=0.5) +
            autolayer(double_exp_model, series = "double exponential", alpha=0.25) +
            autolayer(triple_exp_model, series = "triple exponential", alpha=0.25) +
            autolayer(tbat_model, series = "tbat", alpha=0.7) + 
            guides(colour = guide_legend("Models")) 
    })
    
    #==================
    # Forecast Results/Output
    #==================
    output$forecastOutput <- DT::renderDataTable({
        if (input$segmentInput == 'Profit'){
            business.xts <- xts(x = df$Profit, order.by = df$dateInfo2) 
        } else if (input$segmentInput == 'Turnover'){
            business.xts <- xts(x = df$Turnover, order.by = df$dateInfo2) 
        } else {
            business.xts <- xts(x = df$CustomerCount, order.by = df$dateInfo2) 
        }
        
        business.data <- apply.monthly(business.xts, mean) 
        business.end <- floor(as.numeric(input$traintestInput)*length(business.data)) 
        business.train <- business.data[1:business.end,] 
        business.test <- business.data[(business.end+1):length(business.data),]
        business.start <- c(year (start(business.train)), month(start(business.train)))
        business.end <- c(year(end(business.train)), month(end(business.train)))
        business.train <- ts(as.numeric(business.train), start = business.start, 
                             end = business.end, frequency = as.numeric(input$frequencyInput) )
        business.start <- c(year (start(business.test)), month(start(business.test)))
        business.end <- c(year(end(business.test)), month(end(business.test)))
        business.test <- ts(as.numeric(business.test), start = business.start, 
                            end = business.end, frequency = as.numeric(input$frequencyInput))
        
        
        #set forecast horizon
        forecast.horizon <- as.numeric(input$horizonInput)
        
        # models
        business_train_auto_exp_forecast <- ets(business.train) %>% 
            forecast(h=forecast.horizon)    
        
        business_train_auto_arima_forecast <- auto.arima(business.train) %>% 
            forecast(h=forecast.horizon)             
        
        business_train_simple_exp_forecast <- HoltWinters(business.train,
                                                          beta=FALSE, 
                                                          gamma=FALSE) %>% 
            forecast(h=forecast.horizon)             
        
        business_train_double_exp_forecast <- HoltWinters(business.train,
                                                          beta=TRUE, 
                                                          gamma=FALSE) %>% 
            forecast(h=forecast.horizon)  
        
        business_train_triple_exp_forecast <- HoltWinters(business.train,
                                                          beta=TRUE, 
                                                          gamma=TRUE) %>% 
            forecast(h=forecast.horizon)  
        
        business_train_tbat_forecast <-  tbats(business.train) %>% forecast(h=forecast.horizon)
        
        # forecast output
        auto_exp_forecast <- as.data.frame(business_train_auto_exp_forecast$mean)
        auto_arima_forecast <- as.data.frame(business_train_auto_arima_forecast$mean)
        simple_exp_forecast <- as.data.frame(business_train_simple_exp_forecast$mean)
        double_exp_forecast <- as.data.frame(business_train_double_exp_forecast$mean)
        triple_exp_forecast <- as.data.frame(business_train_triple_exp_forecast$mean)
        tbat_forecast <- as.data.frame(business_train_tbat_forecast$mean)
        
        models <- c("auto-exponential","auto-arima","simple-exponential","double-exponential",
                    "triple-exponential","tbat")
        
        outputInfo <- cbind(auto_exp_forecast,auto_arima_forecast,
                            simple_exp_forecast,double_exp_forecast,
                            triple_exp_forecast,tbat_forecast)
        
        colnames(outputInfo) <- models 
        
        # forecast value output
        DT::datatable(outputInfo, options = list(scrollX = TRUE))
        
    })
    
    #==================
    # Forecast Accuracy
    #==================
    output$accuracyOutput <- DT::renderDataTable({
        if (input$segmentInput == 'Profit'){
            business.xts <- xts(x = df$Profit, order.by = df$dateInfo2) 
        } else if (input$segmentInput == 'Turnover'){
            business.xts <- xts(x = df$Turnover, order.by = df$dateInfo2) 
        } else {
            business.xts <- xts(x = df$CustomerCount, order.by = df$dateInfo2) 
        }
        
        business.data <- apply.monthly(business.xts, mean) 
        business.end <- floor(as.numeric(input$traintestInput)*length(business.data)) 
        business.train <- business.data[1:business.end,] 
        business.test <- business.data[(business.end+1):length(business.data),]
        business.start <- c(year (start(business.train)), month(start(business.train)))
        business.end <- c(year(end(business.train)), month(end(business.train)))
        business.train <- ts(as.numeric(business.train), start = business.start, 
                             end = business.end, frequency = as.numeric(input$frequencyInput) )
        business.start <- c(year (start(business.test)), month(start(business.test)))
        business.end <- c(year(end(business.test)), month(end(business.test)))
        business.test <- ts(as.numeric(business.test), start = business.start, 
                            end = business.end, frequency = as.numeric(input$frequencyInput))

        #set forecast horizon
        forecast.horizon <- as.numeric(input$horizonInput)
        
        # models
        business_train_auto_exp_forecast <- ets(business.train) %>% 
            forecast(h=forecast.horizon)    
        
        business_train_auto_arima_forecast <- auto.arima(business.train) %>% 
            forecast(h=forecast.horizon)             
        
        business_train_simple_exp_forecast <- HoltWinters(business.train,
                                                          beta=FALSE, 
                                                          gamma=FALSE) %>% 
            forecast(h=forecast.horizon)             
        
        business_train_double_exp_forecast <- HoltWinters(business.train,
                                                          beta=TRUE, 
                                                          gamma=FALSE) %>% 
            forecast(h=forecast.horizon)  
        
        business_train_triple_exp_forecast <- HoltWinters(business.train,
                                                          beta=TRUE, 
                                                          gamma=TRUE) %>% 
            forecast(h=forecast.horizon)  
        
        business_train_tbat_forecast <-  tbats(business.train) %>% forecast(h=forecast.horizon) 
        
        
        auto_exp_accuracy <- as.data.frame(accuracy(business_train_auto_exp_forecast ,business.test))
        auto_arima_accuracy <- as.data.frame(accuracy(business_train_auto_arima_forecast ,business.test))
        simple_exp_accuracy <- as.data.frame(accuracy(business_train_simple_exp_forecast ,business.test))
        double_exp_accuracy <- as.data.frame(accuracy(business_train_double_exp_forecast ,business.test))
        triple_exp_accuracy <- as.data.frame(accuracy(business_train_triple_exp_forecast ,business.test))
        tbat_accuracy <- as.data.frame(accuracy(business_train_tbat_forecast ,business.test))
        
        models<- c("auto-exponential","auto-exponential",
                   "auto-arima","auto-arima",
                   "simple-exponential","simple-exponential",
                   "double-exponential","double-exponential",
                   "triple-exponential","triple-exponential",
                   "tbat","tbat")
        
        data<- c("Training set", 'Test set',
                 "Training set", 'Test set',
                 "Training set", 'Test set',
                 "Training set", 'Test set',
                 "Training set", 'Test set',
                 "Training set", 'Test set')
        
        outputInfo <- rbind(auto_exp_accuracy,auto_arima_accuracy,
                            simple_exp_accuracy,double_exp_accuracy,
                            triple_exp_accuracy,tbat_accuracy)  
        
        outputInfo <- cbind(models, data, outputInfo)
        
        # forecast accuracy output
        DT::datatable(outputInfo, options = list(scrollX = TRUE))
        
    })
}

shinyApp(ui, server)