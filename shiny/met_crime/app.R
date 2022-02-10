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
                    'Rape','Bicycle Theft','Other Theft',	'Shoplifting',	'Theft from Person',
                    'Homicide',	'Violence with Injury',	'Violence without Injury')

offences_past <- offences_past %>%
    select(crime_columns)

offences_future <- offences_future %>%
    select(crime_columns)

# convert Month to a Date format
offences_past$Month <- lubridate::my(offences_past$Month)
offences_future$Month <- lubridate::my(offences_future$Month)

#=============
# Drop-downs
#=============
crime_info <- crime_columns[-c(1)]
horizon_info <- c(1:50) 
frequency_info <- c(7, 12, 52, 365)
difference_info <- c("Yes","No")
log_info <- c("Yes","No")

model_info <- c('auto-arima','auto-exponential','simple-exponential',
                'double-exponential','triple-exponential', 'tbat',
                'stl','nnar','combined')

#=============
# Define UI for application
#=============
ui <- dashboardPage(
    dashboardHeader(title = "MET Crime Forecaster"),
    dashboardSidebar(
        sidebarMenu(
            #menuItem("Introduction",tabName = "intro",icon=icon("th")),
            #menuItem("Analysis", tabName = "analysis", icon = icon("th")),
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
                                            h4("Trend Visualization",
                                               style="text-align: center;"),
                                                plotOutput("trendPlot")),
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
            #-----------
            # Forecast
            #-----------
            tabItem(tabName = "forecast",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput('crimeTypeInput',"Crime Type",choices=crime_info,
                                        selected = 'Homicide'),
                            selectInput("horizonInput", "Horizon", 
                                        choices = horizon_info, selected = 14),
                            selectInput("frequencyInput", "Frequency", 
                                        choices = frequency_info, selected = 7),
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
    
    #===============
    # Analysis
    #===============

    #---------------
    # trend plot
    #---------------
    output$trendPlot <- renderPlot({
        crime_name <- as.character(input$crimeTypeInput)
        column_data <- crime_columns[crime_columns %in% c('Month',input$crimeTypeInput)]
        
        offences_df <- offences_past %>%
        select(column_data)
        colnames(offences_df) <- c('DateInfo',"CrimeType")
            
        ggplot(offences_df, aes(x=DateInfo, y=CrimeType)) +
        geom_line() +theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Date", y = crime_name) + 
            theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
    })
    #---------------
    # decomposition plot
    #---------------
    output$decompositionPlot <- renderPlot({
        crime_name <- as.character(input$crimeTypeInput)
        column_data <- crime_columns[crime_columns %in% c('Month',input$crimeTypeInput)]
        df <- offences_past %>%
            select(column_data)
        colnames(df) <- c('DateInfo',"CrimeType")
        
        crime.xts <- xts(x = df$CrimeType, order.by = df$DateInfo) 
        crime.monthly <- apply.monthly(crime.xts, mean) 
        
        crime.end <- floor(1*length(crime.monthly)) 
        crime.data <- crime.monthly[1:crime.end,] 
        crime.start <- c(year (start(crime.data)), month(start(crime.data)))
        crime.end <- c(year(end(crime.data)), month(end(crime.data)))
        crime.data <- ts(as.numeric(crime.data), start = crime.start, 
                           end = crime.end, frequency = as.numeric(input$frequencyInput))  
        
        if (input$differenceInput == "Yes"){
            crime.data <- diff(crime.data, differences = as.numeric(input$differenceNumericInput)) 
        }
        #Decompose the Time Series
        crime.data %>%
            decompose() %>%
            autoplot()
        
    })
    
    #---------------
    # multi season output
    #---------------
    output$multidecompositionPlot <- renderPlot({
        crime_name <- as.character(input$crimeTypeInput)
        column_data <- crime_columns[crime_columns %in% c('Month',input$crimeTypeInput)]
        df <- offences_past %>%
            select(column_data)
        colnames(df) <- c('DateInfo',"CrimeType")
        
        crime.xts <- xts(x = df$CrimeType, order.by = df$DateInfo) 
        crime.monthly <- apply.monthly(crime.xts, mean) 
        
        crime.end <- floor(1*length(crime.monthly)) 
        crime.data <- crime.monthly[1:crime.end,] 
        crime.start <- c(year (start(crime.data)), month(start(crime.data)))
        crime.end <- c(year(end(crime.data)), month(end(crime.data)))
        crime.data <- ts(as.numeric(crime.data), start = crime.start, 
                         end = crime.end, frequency = as.numeric(input$frequencyInput))  
        
        if (input$differenceInput == "Yes"){
            crime.data <- diff(crime.data, differences = as.numeric(input$differenceNumericInput)) 
        }
        #Decompose the Time Series
        crime.data %>%
            mstl() %>%
            autoplot()
    })
    #---------------
    # ACF output
    #---------------
    output$acfPlot <- renderPlot({
        crime_name <- as.character(input$crimeTypeInput)
        column_data <- crime_columns[crime_columns %in% c('Month',input$crimeTypeInput)]
        df <- offences_past %>%
            select(column_data)
        colnames(df) <- c('DateInfo',"CrimeType")
        
        crime.xts <- xts(x = df$CrimeType, order.by = df$DateInfo) 
        crime.monthly <- apply.monthly(crime.xts, mean) 
        
        crime.end <- floor(1*length(crime.monthly)) 
        crime.data <- crime.monthly[1:crime.end,] 
        crime.start <- c(year (start(crime.data)), month(start(crime.data)))
        crime.end <- c(year(end(crime.data)), month(end(crime.data)))
        crime.data <- ts(as.numeric(crime.data), start = crime.start, 
                         end = crime.end, frequency = as.numeric(input$frequencyInput)) 
        
        if (input$logInput == "No"){
            #acf(patient.train)
            ggAcf(crime.data)
        } else {
            ggAcf(log(crime.data))
        }
        
    })
    #---------------
    # PACF output
    #---------------
    output$pacfPlot <- renderPlot({
        crime_name <- as.character(input$crimeTypeInput)
        column_data <- crime_columns[crime_columns %in% c('Month',input$crimeTypeInput)]
        df <- offences_past %>%
            select(column_data)
        colnames(df) <- c('DateInfo',"CrimeType")
        
        crime.xts <- xts(x = df$CrimeType, order.by = df$DateInfo) 
        crime.monthly <- apply.monthly(crime.xts, mean) 
        
        crime.end <- floor(1*length(crime.monthly)) 
        crime.data <- crime.monthly[1:crime.end,] 
        crime.start <- c(year (start(crime.data)), month(start(crime.data)))
        crime.end <- c(year(end(crime.data)), month(end(crime.data)))
        crime.data <- ts(as.numeric(crime.data), start = crime.start, 
                         end = crime.end, frequency = as.numeric(input$frequencyInput)) 
        
        if (input$logInput == "No"){
            ggPacf(crime.data) 
        } else {
            ggPacf(log(crime.data))
        }
    })
    
    #==============
    # Forecast
    #==============
    #----------
    #Forecast Plot
    #----------
    output$forecastPlot <- renderPlot({
        
        crime_name <- as.character(input$crimeTypeInput)
        column_data <- crime_columns[crime_columns %in% c('Month',input$crimeTypeInput)]
        
        train_df <- offences_past %>%
            select(column_data)
        colnames(train_df) <- c('DateInfo',"CrimeType")
        
        test_df <- offences_future %>%
            select(column_data)
        colnames(test_df) <- c('DateInfo',"CrimeType")
        
        crime_train_xts <- xts(x = train_df$CrimeType, order.by = train_df$DateInfo) 
        crime_test_xts <- xts(x = test_df$CrimeType, order.by = test_df$DateInfo) 
        crime.train <- apply.monthly(crime_train_xts, mean) 
        crime.test <- apply.monthly(crime_test_xts, mean) 
        crime.start <- c(year (start(crime.train)), month(start(crime.train)))
        crime.end <- c(year(end(crime.train)), month(end(crime.train)))
        crime.train <- ts(as.numeric(crime.train), start = crime.start, 
                          end = crime.end, frequency = as.numeric(input$frequencyInput))
        crime.start <- c(year (start(crime.test)), month(start(crime.test)))
        crime.end <- c(year(end(crime.test)), month(end(crime.test)))
        crime.test <- ts(as.numeric(crime.test), start = crime.start, 
                         end = crime.end, frequency = as.numeric(input$frequencyInput))
        
        # set forecast horizon
        forecast.horizon <- as.numeric(input$horizonInput)
        
        # models
        auto_exp_model <- crime.train %>% ets %>% forecast(h=forecast.horizon)
        auto_arima_model <- crime.train %>% auto.arima() %>% forecast(h=forecast.horizon)
        simple_exp_model <- crime.train %>% HoltWinters(beta=FALSE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        double_exp_model <- crime.train %>% HoltWinters(beta = TRUE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        triple_exp_model <- crime.train %>% HoltWinters(beta = TRUE, gamma = TRUE) %>% 
            forecast(h=forecast.horizon)
        tbat_model <- crime.train %>% tbats %>% forecast(h=forecast.horizon)
        stl_model <- stlf(crime.train, lambda=0, h=forecast.horizon, biasadj=TRUE)
        nnar_model <- forecast(nnetar(crime.train), h=forecast.horizon)
        combo_model<- (auto_exp_model[["mean"]] + auto_arima_model[["mean"]] +
                           stl_model[["mean"]] + nnar_model[["mean"]] + tbat_model[["mean"]])/5
        
            autoplot(crime.train) +
                autolayer(auto_arima_model,series="auto arima", alpha=0.7) +
                autolayer(auto_exp_model, series = "auto exponential", alpha=0.7) +
                autolayer(simple_exp_model, series= "simple exponential", alpha=0.7) +
                autolayer(double_exp_model, series = "double exponential", alpha=0.7) +
                autolayer(triple_exp_model, series = "triple exponential", alpha=0.7) +
                autolayer(tbat_model, series = "tbat", alpha=0.7) + 
                autolayer(stl_model, series = "stl", alpha=0.7) + 
                autolayer(nnar_model, series = "nnar", alpha=0.7) + 
                autolayer(combo_model, series = "combined", alpha=0.7) + 
                guides(colour = guide_legend("Models")) 
        
    })
    #----------
    #Forecast Output
    #----------
    output$forecastOutput <- DT::renderDataTable({
        crime_name <- as.character(input$crimeTypeInput)
        column_data <- crime_columns[crime_columns %in% c('Month',input$crimeTypeInput)]
        
        train_df <- offences_past %>%
            select(column_data)
        colnames(train_df) <- c('DateInfo',"CrimeType")
        
        test_df <- offences_future %>%
            select(column_data)
        colnames(test_df) <- c('DateInfo',"CrimeType")
        
        crime_train_xts <- xts(x = train_df$CrimeType, order.by = train_df$DateInfo) 
        crime_test_xts <- xts(x = test_df$CrimeType, order.by = test_df$DateInfo) 
        crime.train <- apply.monthly(crime_train_xts, mean) 
        crime.test <- apply.monthly(crime_test_xts, mean) 
        crime.start <- c(year (start(crime.train)), month(start(crime.train)))
        crime.end <- c(year(end(crime.train)), month(end(crime.train)))
        crime.train <- ts(as.numeric(crime.train), start = crime.start, 
                          end = crime.end, frequency = as.numeric(input$frequencyInput))
        crime.start <- c(year (start(crime.test)), month(start(crime.test)))
        crime.end <- c(year(end(crime.test)), month(end(crime.test)))
        crime.test <- ts(as.numeric(crime.test), start = crime.start, 
                         end = crime.end, frequency = as.numeric(input$frequencyInput))
        
        # set forecast horizon
        forecast.horizon <- as.numeric(input$horizonInput)
        
        # models
        auto_exp_model <- crime.train %>% ets %>% forecast(h=forecast.horizon)
        auto_arima_model <- crime.train %>% auto.arima() %>% forecast(h=forecast.horizon)
        simple_exp_model <- crime.train %>% HoltWinters(beta=FALSE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        double_exp_model <- crime.train %>% HoltWinters(beta = TRUE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        triple_exp_model <- crime.train %>% HoltWinters(beta = TRUE, gamma = TRUE) %>% 
            forecast(h=forecast.horizon)
        tbat_model <- crime.train %>% tbats %>% forecast(h=forecast.horizon)
        stl_model <- stlf(crime.train, lambda=0, h=forecast.horizon, biasadj=TRUE)
        nnar_model <- forecast(nnetar(crime.train), h=forecast.horizon)
        combo_model<- (auto_exp_model[["mean"]] + auto_arima_model[["mean"]] +
                           stl_model[["mean"]] + nnar_model[["mean"]] + tbat_model[["mean"]])/5
        
        
        # forecast output
        auto_exp_forecast <- as.data.frame(auto_exp_model$mean)
        auto_arima_forecast <- as.data.frame(auto_arima_model$mean)
        simple_exp_forecast <- as.data.frame(simple_exp_model$mean)
        double_exp_forecast <- as.data.frame(double_exp_model$mean)
        triple_exp_forecast <- as.data.frame(triple_exp_model$mean)
        tbat_forecast <- as.data.frame(tbat_model$mean)
        stl_forecast <- as.data.frame(stl_model$mean)
        nnar_forecast <- as.data.frame(nnar_model$mean)
        combo_forecast <- as.data.frame(combo_model)
        
        numeric_update <- function(df){
            rownames(df) <- c()
            is.num <- sapply(df, is.numeric)
            df[is.num] <- lapply(df[is.num], round, 0)           
            return (df)
        }
            
            auto_exp_forecast <- numeric_update(auto_exp_forecast)
            auto_arima_forecast <- numeric_update(auto_arima_forecast)
            simple_exp_forecast <- numeric_update(simple_exp_forecast)
            double_exp_forecast <- numeric_update(double_exp_forecast)
            triple_exp_forecast <- numeric_update(triple_exp_forecast)
            tbat_forecast <- numeric_update(tbat_forecast)
            stl_forecast <- numeric_update(stl_forecast)
            nnar_forecast <- numeric_update(nnar_forecast)
            combo_forecast <- numeric_update(combo_forecast)
            
            outputInfo <- cbind(auto_exp_forecast,auto_arima_forecast,
                                simple_exp_forecast,double_exp_forecast,
                                triple_exp_forecast,tbat_forecast,
                                stl_forecast,nnar_forecast,combo_forecast)
            
            colnames(outputInfo) <- model_info
            
            # forecast value output
            DT::datatable(outputInfo, options = list(scrollX = TRUE))
        
    })
    
    #--------------
    # Forecast Accuracy
    #--------------
    output$accuracyOutput <- DT::renderDataTable({
        
        crime_name <- as.character(input$crimeTypeInput)
        column_data <- crime_columns[crime_columns %in% c('Month',input$crimeTypeInput)]
        
        train_df <- offences_past %>%
            select(column_data)
        colnames(train_df) <- c('DateInfo',"CrimeType")
        
        test_df <- offences_future %>%
            select(column_data)
        colnames(test_df) <- c('DateInfo',"CrimeType")
        
        crime_train_xts <- xts(x = train_df$CrimeType, order.by = train_df$DateInfo) 
        crime_test_xts <- xts(x = test_df$CrimeType, order.by = test_df$DateInfo) 
        crime.train <- apply.monthly(crime_train_xts, mean) 
        crime.test <- apply.monthly(crime_test_xts, mean) 
        crime.start <- c(year (start(crime.train)), month(start(crime.train)))
        crime.end <- c(year(end(crime.train)), month(end(crime.train)))
        crime.train <- ts(as.numeric(crime.train), start = crime.start, 
                          end = crime.end, frequency = as.numeric(input$frequencyInput))
        crime.start <- c(year (start(crime.test)), month(start(crime.test)))
        crime.end <- c(year(end(crime.test)), month(end(crime.test)))
        crime.test <- ts(as.numeric(crime.test), start = crime.start, 
                         end = crime.end, frequency = as.numeric(input$frequencyInput))
        
        # set forecast horizon
        forecast.horizon <- as.numeric(input$horizonInput)
        
        # models
        auto_exp_model <- crime.train %>% ets %>% forecast(h=forecast.horizon)
        auto_arima_model <- crime.train %>% auto.arima() %>% forecast(h=forecast.horizon)
        simple_exp_model <- crime.train %>% HoltWinters(beta=FALSE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        double_exp_model <- crime.train %>% HoltWinters(beta = TRUE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        triple_exp_model <- crime.train %>% HoltWinters(beta = TRUE, gamma = TRUE) %>% 
            forecast(h=forecast.horizon)
        tbat_model <- crime.train %>% tbats %>% forecast(h=forecast.horizon)
        stl_model <- stlf(crime.train, lambda=0, h=forecast.horizon, biasadj=TRUE)
        nnar_model <- forecast(nnetar(crime.train), h=forecast.horizon)
        combo_model<- (auto_exp_model[["mean"]] + auto_arima_model[["mean"]] +
                           stl_model[["mean"]] + nnar_model[["mean"]] + tbat_model[["mean"]])/5
        
        
       
        
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)