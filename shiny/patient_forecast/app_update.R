# Patient Forecast
rm(list = ls()) #clear environment
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse','readxl',
              'shiny','shinydashboard','scales','dplyr','mlbench','caTools',
              'forecast','TTR','xts','lubridate')
#=============
# load packages
#=============
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
} else {
    #message("File doesn't exist, please check")
}


#=============
# dropdowns
#=============
aggregate_info <- c("daily",'weekly','monthly')
horizon_info <- c(1:50) #default 14
frequency_info <- c(7, 12, 52, 365)
difference_info <- c("Yes","No")
log_info <- c("Yes","No")
model_info <- c('auto-arima','auto-exponential','simple-exponential',
                'double-exponential','triple-exponential', 'tbat','manual-arima')

#=============
# Define UI for application
#=============
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
            patient.data <- patient.monthly[1:patient.end,] 
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
    
    # Forecast Visualization
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
        
        # set forecast horizon
        forecast.horizon <- as.numeric(input$horizonInput)
        
        # models
        auto_exp_model <- patient.train %>% ets %>% forecast(h=forecast.horizon)
        auto_arima_model <- patient.train %>% auto.arima() %>% forecast(h=forecast.horizon)
        simple_exp_model <- patient.train %>% HoltWinters(beta=FALSE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        double_exp_model <- patient.train %>% HoltWinters(beta = TRUE, gamma=FALSE) %>% 
            forecast(h=forecast.horizon)
        triple_exp_model <- patient.train %>% HoltWinters(beta = TRUE, gamma = TRUE) %>% 
            forecast(h=forecast.horizon)
        tbat_model <- patient.train %>% tbats %>% forecast(h=forecast.horizon)
        # manual arima info
        #orderinfo = c(as.numeric(input$autoInput),as.numeric(input$difference2Input),
        #              as.numeric(input$maInput))
        # manual_arima_model <- patient.train %>% Arima(order=orderinfo) 
        
        # model output
        auto_arima <- "auto-arima"        %in% input$modelInput
        auto_exp   <- 'auto-exponential'  %in% input$modelInput
        simple_exp <- "simple-exponential" %in% input$modelInput
        double_exp <- "double-exponential" %in% input$modelInput
        triple_exp <- "triple-exponential" %in% input$modelInput
        tbat <- "tbat"  %in% input$modelInput
        #manual_arima <- "manual-arima"  %in% input$modelInput
        
        model_selection <- unlist(strsplit(input$modelInput, split=" "))
        model_count <- length(model_selection)
        
        if (is.null(input$modelInput)){
            
        } else if (model_count == 1){
            if (auto_arima){
                auto_arima_model %>% autoplot()+ 
                    guides(colour = guide_legend("Models"))
            } else if (auto_exp) {
                auto_exp_model %>% autoplot()+ 
                    guides(colour = guide_legend("Models"))
            } else if (simple_exp) {
                simple_exp_model %>% autoplot()+ 
                    guides(colour = guide_legend("Models"))
            } else if (double_exp) {
                double_exp_model %>% autoplot()+ 
                    guides(colour = guide_legend("Models"))
            } else if (triple_exp) {
                triple_exp_model %>% autoplot()+ 
                    guides(colour = guide_legend("Models"))
            } else if (tbat ) {
                tbat_model %>% autoplot() + 
                    guides(colour = guide_legend("Models"))  
            }
        } else if (model_count == 2){
            if(auto_arima &  auto_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model,series="auto arima", alpha=0.5) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) + 
                    guides(colour = guide_legend("Models"))
            } else if(auto_arima &  simple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model,series="auto arima",alpha=0.5) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  double_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model,series="auto arima",alpha=0.5) +
                    autolayer(double_exp_model, series = "double exponential",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model,series="auto arima",alpha=0.5) +
                    autolayer(triple_exp_model, series = "triple exponential",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model,series="auto arima",alpha=0.5) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_exp &  simple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series = "auto exponential",alpha=0.5) +
                    autolayer(simple_exp_model,series="simple exponential",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_exp &  double_exp) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series = "auto exponential",alpha=0.5) +
                    autolayer(double_exp_model,series="double exponential",alpha=0.5)  + 
                    guides(colour = guide_legend("Models"))               
            }
            else if(auto_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series = "auto exponential",alpha=0.5) +
                    autolayer(triple_exp_model,series="triple exponential",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series = "auto exponential",alpha=0.5) +
                    autolayer(tbat_model,series="tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(simple_exp &  double_exp) {
                autoplot(patient.train) +
                    autolayer(simple_exp_model,series="simple exponential",alpha=0.5) + 
                    autolayer(double_exp_model,series="double exponential",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(simple_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(simple_exp_model,series="simple exponential",alpha=0.5) + 
                    autolayer(triple_exp_model,series="triple exponential",alpha=0.5)   + 
                    guides(colour = guide_legend("Models"))              
            }
            else if(simple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(simple_exp_model,series="simple exponential",alpha=0.5) + 
                    autolayer(tbat_model,series="tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(double_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(double_exp_model,series="double exponential",alpha=0.5) + 
                    autolayer(triple_exp_model,series="triple exponential",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(double_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(double_exp_model,series="double exponential",alpha=0.5) + 
                    autolayer(tbat_model,series="tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(triple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(triple_exp_model,series="triple exponential",alpha=0.5) + 
                    autolayer(tbat_model,series="tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
        } else if (model_count == 3) {
            if(auto_arima &  auto_exp &  simple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series = "auto arima",alpha=0.5) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(simple_exp_model, series= "simple exponential", alpha=0.2) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  auto_exp &  double_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series = "auto arima",alpha=0.5) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(double_exp_model, series= "double exponential",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  auto_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series = "auto arima",alpha=0.5) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(triple_exp_model, series= "triple exponential",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  auto_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series = "auto arima",alpha=0.5) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(tbat_model, series = "tbat", alpha=0.7)  + 
                    guides(colour = guide_legend("Models"))              
            }
            else if(auto_arima &  simple_exp &  double_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series = "auto arima",alpha=0.5) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) +
                    autolayer(double_exp_model, series= "double exponential",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  simple_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series = "auto arima",alpha=0.5) +
                    autolayer(simple_exp_model, series= "simple exponential", alpha=0.2) + 
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  simple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series = "auto arima",alpha=0.5) +
                    autolayer(simple_exp_model, series= "simple exponential", alpha=0.2) + 
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  double_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series = "auto arima",alpha=0.5) +
                    autolayer(double_exp_model, series= "double exponential",alpha=0.5) + 
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  double_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series = "auto arima",alpha=0.5) +
                    autolayer(double_exp_model, series= "double exponential",alpha=0.5) + 
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  triple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series = "auto arima",alpha=0.5) +
                    autolayer(triple_exp_model, series= "triple exponential",alpha=0.5) + 
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_exp &  simple_exp &  double_exp) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) + 
                    autolayer(double_exp_model, series= "double exponential",alpha=0.5)  + 
                    guides(colour = guide_legend("Models"))               
            }
            else if(auto_exp &  simple_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) + 
                    autolayer(triple_exp_model, series= "triple exponential",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_exp &  simple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) + 
                    autolayer(tbat_model, series = "tbat", alpha=0.7)     + 
                    guides(colour = guide_legend("Models"))           
            }
            else if(auto_exp &  double_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) + 
                    autolayer(triple_exp_model, series= "triple exponential",alpha=0.5)+ 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_exp &  double_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(double_exp_model, series= "double exponential",alpha=0.5) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_exp &  triple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(triple_exp_model, series= "triple exponential",alpha=0.5) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(simple_exp &  double_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) +
                    autolayer(double_exp_model, series= "double exponential",alpha=0.5) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(simple_exp &  double_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) +
                    autolayer(double_exp_model, series= "double exponential",alpha=0.5) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(simple_exp &  triple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(simple_exp_model, series= "simple exponential", alpha=0.2) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(double_exp &  triple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(double_exp_model, series= "double exponential",alpha=0.5) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
        } else if (model_count == 4){
            if(auto_arima &  auto_exp &  simple_exp &  double_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series= "auto arima",alpha=0.5) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) +
                    autolayer(double_exp_model,series="double exponential", alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  auto_exp &  simple_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series= "auto arima",alpha=0.5) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(simple_exp_model,series="simple exponential",alpha=0.5) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2)  + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  auto_exp &  simple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series= "auto arima",alpha=0.5) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(simple_exp_model,series="simple exponential",alpha=0.5) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5)  + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  auto_exp &  double_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series= "auto arima",alpha=0.5) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(double_exp_model,series="double exponential",alpha=0.5) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2)  + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  auto_exp &  double_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series= "auto arima",alpha=0.5) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) + 
                    autolayer(tbat_model,series="tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  auto_exp &  triple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series= "auto arima",alpha=0.5) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) + 
                    autolayer(tbat_model,series="tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_arima &  simple_exp &  double_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series= "auto arima",alpha=0.5) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) +
                    autolayer(double_exp_model,series="double exponential",alpha=0.5) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) + 
                    guides(colour = guide_legend("Models"))
                
            }
            else if(auto_arima &  simple_exp &  double_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series= "auto arima",alpha=0.5) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) + 
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) +
                    autolayer(tbat_model,series="tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models")) 
            }
            else if(auto_arima &  simple_exp &  triple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series= "auto arima",alpha=0.5) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) + 
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) +
                    autolayer(tbat_model,series="tbat",alpha=0.5)   + 
                    guides(colour = guide_legend("Models"))             
            }
            else if(auto_arima &  double_exp &  triple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series= "auto arima",alpha=0.5) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) + 
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) +
                    autolayer(tbat_model,series="tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_exp &  simple_exp &  double_exp &  triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series= "auto exponential",alpha=0.5) +
                    autolayer(simple_exp_model,series="simple exponential",alpha=0.5) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) + 
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_exp &  simple_exp &  double_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series= "auto exponential",alpha=0.5) +
                    autolayer(simple_exp_model,series="simple exponential",alpha=0.5) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) + 
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_exp &  simple_exp &  triple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series= "auto exponential",alpha=0.5) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) + 
                    autolayer(triple_exp_model,series="triple exponential",alpha=0.5) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(auto_exp &  double_exp &  triple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(auto_exp_model, series= "auto exponential",alpha=0.5) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) + 
                    autolayer(triple_exp_model,series="triple exponential",alpha=0.5) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
            else if(simple_exp &  double_exp &  triple_exp &  tbat) {
                autoplot(patient.train) +
                    autolayer(simple_exp_model, series= "simple exponential", alpha=0.2) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) + 
                    autolayer(triple_exp_model,series="triple exponential",alpha=0.5) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5) + 
                    guides(colour = guide_legend("Models"))
            }
        } else if (model_count == 5){
            if (auto_arima & auto_exp & simple_exp & double_exp & triple_exp) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model,series="auto arima", alpha=0.2) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(simple_exp_model, series= "simple exponential", alpha=0.2) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) + 
                    guides(colour = guide_legend("Models"))
            } else if (auto_arima & simple_exp & double_exp & triple_exp & tbat) {
                autoplot(patient.train) +
                    autolayer(auto_arima_model, series = "auto arima",alpha=0.5) +
                    autolayer(simple_exp_model, series= "simple exponential", alpha=0.2) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5)+ 
                    guides(colour = guide_legend("Models"))
            } else if (auto_arima & auto_exp & simple_exp & double_exp & tbat){
                autoplot(patient.train) +
                    autolayer(auto_arima_model,series="auto arima", alpha=0.2) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(simple_exp_model, series= "simple exponential", alpha=0.2) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) +
                    autolayer(tbat_model, series = "tbat",alpha=0.5)  + 
                    guides(colour = guide_legend("Models"))              
            } else if (auto_arima & auto_exp & simple_exp &  triple_exp & tbat){
                autoplot(patient.train) +
                    autolayer(auto_arima_model,series="auto arima", alpha=0.2) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(simple_exp_model, series= "simple exponential", alpha=0.2) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2)  + 
                    autolayer(tbat_model, series = "tbat", alpha=0.7)  + 
                    guides(colour = guide_legend("Models"))            
            } else if (auto_arima & auto_exp & double_exp & triple_exp & tbat){
                autoplot(patient.train) +
                    autolayer(auto_arima_model,series="auto arima", alpha=0.2) +
                    autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) + 
                    autolayer(tbat_model, series = "tbat", alpha=0.7)+ 
                    guides(colour = guide_legend("Models"))
            }    else if (auto_exp & simple_exp & double_exp & triple_exp & tbat){
                autoplot(patient.train) +
                    autolayer(auto_exp_model,series="auto exp",alpha=0.5) +
                    autolayer(simple_exp_model, series = "simple exponential",alpha=0.5) +
                    autolayer(double_exp_model, series = "double exponential", alpha=0.7) +
                    autolayer(triple_exp_model, series = "triple exponential", alpha=0.2) + 
                    autolayer(tbat_model, series = "tbat", alpha=0.7)+ 
                    guides(colour = guide_legend("Models"))
            }
            
        } else {
            autoplot(patient.train) +
                autolayer(auto_arima_model,series="auto arima", alpha=0.2) +
                autolayer(auto_exp_model, series = "auto exponential", alpha=0.2) +
                autolayer(simple_exp_model, series= "simple exponential", alpha=0.5) +
                autolayer(double_exp_model, series = "double exponential", alpha=0.25) +
                autolayer(triple_exp_model, series = "triple exponential", alpha=0.25) +
                autolayer(tbat_model, series = "tbat", alpha=0.7) + 
                guides(colour = guide_legend("Models")) #+
            #autolayer(manual_arima_model, series = "manual arima")            
        }
        
    })

    # Forecast Values
    output$forecastOutput <- DT::renderDataTable({
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
        patient_train_auto_exp_forecast <- ets(patient.train) %>% 
            forecast(h=forecast.horizon)    
        
        patient_train_auto_arima_forecast <- auto.arima(patient.train) %>% 
            forecast(h=forecast.horizon)             
        
        patient_train_simple_exp_forecast <- HoltWinters(patient.train,
                                                         beta=FALSE, 
                                                         gamma=FALSE) %>% 
            forecast(h=forecast.horizon)             
        
        patient_train_double_exp_forecast <- HoltWinters(patient.train,
                                                         beta=TRUE, 
                                                         gamma=FALSE) %>% 
            forecast(h=forecast.horizon)  
        
        patient_train_triple_exp_forecast <- HoltWinters(patient.train,
                                                         beta=TRUE, 
                                                         gamma=TRUE) %>% 
            forecast(h=forecast.horizon)  
        
        patient_train_tbat_forecast <-  tbats(patient.train) %>% forecast(h=forecast.horizon)
        
        patient_train_manual_forecast <- Arima(patient.train, 
                                               c(as.numeric(input$autoInput), 
                                                 as.numeric(input$difference2Input),
                                                 as.numeric(input$maInput)))
        
        # forecast output
        auto_exp_forecast <- as.data.frame(patient_train_auto_exp_forecast$mean)
        auto_arima_forecast <- as.data.frame(patient_train_auto_arima_forecast$mean)
        simple_exp_forecast <- as.data.frame(patient_train_simple_exp_forecast$mean)
        double_exp_forecast <- as.data.frame(patient_train_double_exp_forecast$mean)
        triple_exp_forecast <- as.data.frame(patient_train_triple_exp_forecast$mean)
        tbat_forecast <- as.data.frame(patient_train_tbat_forecast$mean)
        manual_arima_forecast <- as.data.frame(patient_train_manual_forecast$mean)
        
        numeric_update <- function(df){
            rownames(df) <- c()
            is.num <- sapply(df, is.numeric)
            df[is.num] <- lapply(df[is.num], round, 0)           
            return (df)
        }
        
        # Placeholder for manual arima check
        if(length(manual_arima_forecast) < 1){
            auto_exp_forecast <- numeric_update(auto_exp_forecast)
            auto_arima_forecast <- numeric_update(auto_arima_forecast)
            simple_exp_forecast <- numeric_update(simple_exp_forecast)
            double_exp_forecast <- numeric_update(double_exp_forecast)
            triple_exp_forecast <- numeric_update(triple_exp_forecast)
            tbat_forecast <- numeric_update(tbat_forecast)
            #manual_arima_forecast <- numeric_update(manual_arima_forecast)
            
            models <- c("auto-exponential","auto-arima","simple-exponential","double-exponential",
                        "triple-exponential","tbat")
            
            outputInfo <- cbind(auto_exp_forecast,auto_arima_forecast,
                                simple_exp_forecast,double_exp_forecast,
                                triple_exp_forecast,tbat_forecast)
            
            colnames(outputInfo) <- models 
        } else {
            auto_exp_forecast <- numeric_update(auto_exp_forecast)
            auto_arima_forecast <- numeric_update(auto_arima_forecast)
            simple_exp_forecast <- numeric_update(simple_exp_forecast)
            double_exp_forecast <- numeric_update(double_exp_forecast)
            triple_exp_forecast <- numeric_update(triple_exp_forecast)
            tbat_forecast <- numeric_update(tbat_forecast)
            manual_arima_forecast <- numeric_update(manual_arima_forecast)
            
            models <- c("auto-exponential","auto-arima","simple-exponential","double-exponential",
                        "triple-exponential","tbat", "manual-arima")
            
            outputInfo <- cbind(auto_exp_forecast,auto_arima_forecast,
                                simple_exp_forecast,double_exp_forecast,
                                triple_exp_forecast,tbat_forecast, manual_arima_forecast)
            
            colnames(outputInfo) <- models             
        }
        
        
        
        # model output
        auto_arima <- "auto-arima"        %in% input$modelInput
        auto_exp   <- 'auto-exponential'  %in% input$modelInput
        simple_exp <- "simple-exponential" %in% input$modelInput
        double_exp <- "double-exponential" %in% input$modelInput
        triple_exp <- "triple-exponential" %in% input$modelInput
        tbat       <- "tbat"  %in% input$modelInput
        manual_arima <- "manual-arima"  %in% input$modelInput
        
        model_selection <- unlist(strsplit(input$modelInput, split=" "))
        model_count <- length(model_selection)
        

        # models selection
        if (is.null(input$modelInput)){

        } else if (model_count == 1){
            if (auto_arima){
                outputInfo <- outputInfo %>%
                    select(auto-arima)
            } else if (auto_exp) {
                outputInfo <- outputInfo %>%
                    select(auto-exponential)
            } else if (simple_exp) {
                outputInfo <- outputInfo %>%
                    select(simple-exponential)
            } else if (double_exp) {
                outputInfo <- outputInfo %>%
                    select(double-exponential)
            } else if (triple_exp) {
                outputInfo <- outputInfo %>%
                    select(triple-exponential)
            } else if (tbat ) {
                outputInfo <- outputInfo %>%
                    select(tbat)
            } else if (manual_arima){
                outputInfo <- outputInfo %>%
                    select(manual-arima)
            }
        } else if (model_count == 2){
            if(auto_arima &  auto_exp) {
                outputInfo <- outputInfo %>%
                    select(auto-arima,auto-exponential)
            } else if(auto_arima &  simple_exp) {
                outputInfo <- outputInfo %>%
                    select(auto-arima,simple-exponential)
            } else if(auto_arima &  double_exp) {
                outputInfo <- outputInfo %>%
                    select(auto-arima,double-exponential)
            } else if(auto_arima &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(auto-arima,triple-exponential)
            } else if(auto_arima &  tbat) {
                outputInfo <- outputInfo %>%
                    select(auto-arima,tbat)
            } else if(auto_exp &  simple_exp) {
                outputInfo <- outputInfo %>%
                    select(auto-exponential,simple-exponential)
            } else if(auto_exp &  double_exp) {
                outputInfo <- outputInfo %>%
                    select(auto-exponential,double-exponential)
            }else if(auto_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(auto-exponential,triple-exponential)
            } else if(auto_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(tbat,auto-exponential)
            }else if(simple_exp &  double_exp) {
                outputInfo <- outputInfo %>%
                    select(simple-exponential,double-exponential)
            }else if(simple_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(simple-exponential,triple-exponential)
            }else if(simple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(tbat,simple-exponential)
            }else if(double_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(double-exponential,triple-exponential)
            }else if(double_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(tbat,double-exponential)
            }else if(triple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(tbat,triple-exponential)
            }else if(simple_exp &  manual_arima) {
                outputInfo <- outputInfo %>%
                    select(simple-exp,triple-exponential)
            }else if(double_exp  &  manual_arima) {
                outputInfo <- outputInfo %>%
                    select(manual-arima,double-exponential)
            }else if(triple_exp &  manual_arima) {
                outputInfo <- outputInfo %>%
                    select(manual-arima,triple-exponential)
            }else if(auto_exp &  manual_arima) {
                outputInfo <- outputInfo %>%
                    select(auto-exponential,manual-arima)
            }else if(auto_arima &  manual_arima) {
                outputInfo <- outputInfo %>%
                    select(auto-arima,manual-arima)
            }else if(tbat & manual_arima) {
                outputInfo <- outputInfo %>%
                    select(tbat,manual-arima)
            }


        } else if (model_count == 3) {
            if(auto_arima &  auto_exp &  simple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat,double-exponential,triple-exponential))
            }else if(auto_arima &  auto_exp &  double_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat,simple-exponential,triple-exponential))
            }else if(auto_arima &  auto_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat,simple-exponential,double-exponential))
            }else if(auto_arima &  auto_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(triple-exponential,double-exponential,simple-exponential))
            }else if(auto_arima &  simple_exp &  double_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat,auto-exponential,triple-exponential))
            }else if(auto_arima &  simple_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat,double-exponential,auto-exponential))
            }else if(auto_arima &  simple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(double_exponential,auto-exponential,triple-exponential))
            }else if(auto_arima &  double_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat,auto-exponential,simple-exponential))
            }else if(auto_arima &  double_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(simple-exponential,auto-exponential,triple-exponential))
            }else if(auto_arima &  triple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(double-exponential,auto-exponential,simple-exponential))
            }else if(auto_exp &  simple_exp &  double_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,tbat,triple-exponential))
            }else if(auto_exp &  simple_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,tbat,double-exponential))
            }else if(auto_exp &  simple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,double-exponential,triple-exponential))
            }else if(auto_exp &  double_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,tbat,simple-exponential))
            }else if(auto_exp &  double_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,triple-exponential,simple-exponential))
            }else if(auto_exp &  triple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,double-exponential,simple-exponential))
            }else if(simple_exp &  double_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,auto-exponential,tbat))
            }else if(simple_exp &  double_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,auto-exponential,triple-exponential))
            }else if(simple_exp &  triple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,auto-exponential,double-exponential))
            }else if(double_exp &  triple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,auto-exponential,simple-exponential))
            } else if(auto_exp & auto_arima & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, manual-arima) )
            }else if(auto_exp & simple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, simple-exponential, manual-arima) )
            }else if(auto_exp & double_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, double-exponential, manual-arima) )
            }else if(auto_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, triple-exponential, manual-arima) )
            }else if(auto_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, tbat, manual-arima) )
            }else if(auto_arima & simple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, simple-exponential, manual-arima) )
            }else if(auto_arima & double_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, double-exponential, manual-arima) )
            }else if(auto_arima & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, triple-exponential, manual-arima) )
            }else if(auto_arima & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, tbat, manual-arima) )
            }else if(simple_exp & double_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(simple-exponential, double-exponential, manual-arima) )
            }else if(simple_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(simple-exponential, triple-exponential, manual-arima) )
            }else if(simple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(simple-exponential, tbat, manual-arima) )
            }else if(double_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(double-exponential, triple-exponential, manual-arima) )
            }else if(double_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(double-exponential, tbat, manual-arima) )
            }else if(triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(triple-exponential, tbat, manual-arima) )
            }


        } else if (model_count == 4){
            if(auto_arima &  auto_exp &  simple_exp &  double_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat,triple-exponential))
            }else if(auto_arima &  auto_exp &  simple_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat,double-exponential))
            }else if(auto_arima &  auto_exp &  simple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(double-exponential,triple-exponential))
            }else if(auto_arima &  auto_exp &  double_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat,simple-exponential))
            }else if(auto_arima &  auto_exp &  double_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(simple-exponential,triple-exponential))
            }else if(auto_arima &  auto_exp &  triple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(simple-exponential,double-exponential))
            }else if(auto_arima &  simple_exp &  double_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat,auto-exponential))
            }else if(auto_arima &  simple_exp &  double_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(triple-exponential,auto-exponential))
            }else if(auto_arima &  simple_exp &  triple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(double-exponential,auto-exponential))
            }else if(auto_arima &  double_exp &  triple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(simple-exponential,auto-exponential))
            }else if(auto_exp &  simple_exp &  double_exp &  triple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat,auto-exponential))
            }else if(auto_exp &  simple_exp &  double_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,triple-exponential))
            }else if(auto_exp &  simple_exp &  triple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,double-exponential))
            }else if(auto_exp &  double_exp &  triple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,simple-exponential))
            }else if(simple_exp &  double_exp &  triple_exp &  tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima,auto-exponential))
            }else if(auto_exp & auto_arima & simple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, simple-exponential, manual-arima) )
            }else if(auto_exp & auto_arima & double_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, double-exponential, manual-arima) )
            }else if(auto_exp & auto_arima & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, triple-exponential, manual-arima) )
            }else if(auto_exp & auto_arima & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, tbat, manual-arima) )
            }else if(auto_exp & simple_exp & double_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, simple-exponential, double-exponential, manual-arima) )
            }else if(auto_exp & simple_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, simple-exponential, triple-exponential, manual-arima) )
            }else if(auto_exp & simple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, simple-exponential, tbat, manual-arima) )
            }else if(auto_exp & double_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, double-exponential, triple-exponential, manual-arima) )
            }else if(auto_exp & double_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, double-exponential, tbat, manual-arima) )
            }else if(auto_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, triple-exponential, tbat, manual-arima) )
            }else if(auto_arima & simple_exp & double_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, simple-exponential, double-exponential, manual-arima) )
            }else if(auto_arima & simple_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, simple-exponential, triple-exponential, manual-arima) )
            }else if(auto_arima & simple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, simple-exponential, tbat, manual-arima) )
            }else if(auto_arima & double_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, double-exponential, triple-exponential, manual-arima) )
            }else if(auto_arima & double_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, double-exponential, tbat, manual-arima) )
            }else if(auto_arima & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, triple-exponential, tbat, manual-arima) )
            }else if(simple_exp & double_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(simple-exponential, double-exponential, triple-exponential, manual-arima) )
            }else if(simple_exp & double_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(simple-exponential, double-exponential, tbat, manual-arima) )
            }else if(simple_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(simple-exponential, triple-exponential, tbat, manual-arima) )
            }else if(double_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(double-exponential, triple-exponential, tbat, manual-arima) )
            }

        } else if (model_count == 5){
            if (auto_arima & auto_exp & simple_exp & double_exp & triple_exp) {
                outputInfo <- outputInfo %>%
                    select(!c(tbat))
            } else if (auto_arima & simple_exp & double_exp & triple_exp & tbat) {
                outputInfo <- outputInfo %>%
                    select(!c(auto-exponential))
            } else if (auto_arima & auto_exp & simple_exp & double_exp & tbat){
                outputInfo <- outputInfo %>%
                    select(!c(triple-exponential)  )
            } else if (auto_arima & auto_exp & simple_exp &  triple_exp & tbat){
                outputInfo <- outputInfo %>%
                    select(!c(double-exponential))
            } else if (auto_arima & auto_exp & double_exp & triple_exp & tbat){
                outputInfo <- outputInfo %>%
                    select(!c(simple-exponential))
            } else if (auto_exp & simple_exp & double_exp & triple_exp & tbat){
                outputInfo <- outputInfo %>%
                    select(!c(auto-arima))
            } else if (auto_exp & auto_arima & simple_exp & double_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, simple-exponential, double-exponential, manual-arima))
            } else if(auto_exp & auto_arima & simple_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, simple-exponential, triple-exponential, manual-arima))
            } else if(auto_exp & auto_arima & simple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, simple-exponential, tbat, manual-arima))
            } else if(auto_exp & auto_arima & double_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, double-exponential, triple-exponential, manual-arima))
            } else if(auto_exp & auto_arima & double_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, double-exponential, tbat, manual-arima))
            } else if(auto_exp & auto_arima & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, triple-exponential, tbat, manual-arima))
            } else if(auto_exp & simple_exp & double_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, simple-exponential, double-exponential, triple-exponential, manual-arima))
            } else if(auto_exp & simple_exp & double_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, simple-exponential, double-exponential, tbat, manual-arima))
            } else if(auto_exp & simple_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, simple-exponential, triple-exponential, tbat, manual-arima))
            } else if(auto_exp & double_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, double-exponential, triple-exponential, tbat, manual-arima))
            } else if(auto_arima & simple_exp & double_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, simple-exponential, double-exponential, triple-exponential, manual-arima))
            } else if(auto_arima & simple_exp & double_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, simple-exponential, double-exponential, tbat, manual-arima))
            } else if(auto_arima & simple_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, simple-exponential, triple-exponential, tbat, manual-arima))
            } else if(auto_arima & double_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, double-exponential, triple-exponential, tbat, manual-arima))
            } else if(simple_exp & double_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(simple-exponential, double-exponential,
                             triple-exponential, tbat, manual-arima))
            }
        } else if (model_count == 6){
            if (auto_exp & auto_arima & simple_exp & double_exp & triple_exp & tbat){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, simple-exponential,
                             double-exponential, triple-exponential, tbat))
            } else if (auto_exp & auto_arima & simple_exp & double_exp & triple_exp & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, simple-exponential,
                             double-exponential, triple-exponential, manual-arima))
            } else if(auto_exp & auto_arima & simple_exp & double_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, simple-exponential,
                             double-exponential, tbat, manual-arima) )
            } else if(auto_exp & auto_arima & simple_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, simple-exponential,
                             triple-exponential, tbat, manual-arima))
            } else if(auto_exp & auto_arima & double_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, auto-arima, double-exponential,
                             triple-exponential, tbat, manual-arima))
            } else if(auto_exp & simple_exp & double_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-exponential, simple-exponential, double-exponential,
                             triple-exponential, tbat, manual-arima))
            } else if(auto_arima & simple_exp & double_exp & triple_exp & tbat & manual_arima){
                outputInfo <- outputInfo %>%
                    select(c(auto-arima, simple-exponential, double-exponential, triple-exponential,
                             tbat, manual-arima))
            }
        }
        
        # forecast value output
        DT::datatable(outputInfo, options = list(scrollX = TRUE))
    })
       
    # Forecast Accuracy
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
        patient_train_auto_exp_forecast <- ets(patient.train) %>% 
                 forecast(h=forecast.horizon)    
        
        patient_train_auto_arima_forecast <- auto.arima(patient.train) %>% 
                 forecast(h=forecast.horizon)             
        
        patient_train_simple_exp_forecast <- HoltWinters(patient.train,
                                                   beta=FALSE, 
                                                   gamma=FALSE) %>% 
                 forecast(h=forecast.horizon)             
        
        patient_train_double_exp_forecast <- HoltWinters(patient.train,
                                                   beta=TRUE, 
                                                   gamma=FALSE) %>% 
                 forecast(h=forecast.horizon)  
        
        patient_train_triple_exp_forecast <- HoltWinters(patient.train,
                                                   beta=TRUE, 
                                                   gamma=TRUE) %>% 
                 forecast(h=forecast.horizon)  
        
        patient_train_tbat_forecast <-  tbats(patient.train) %>% forecast(h=forecast.horizon)
        #manual arima forecast
        patient_train_manual_forecast <-  Arima(patient.train,c(as.numeric(input$autoInput), 
                                                                as.numeric(input$difference2Input),
                                                                as.numeric(input$maInput))) %>% 
            forecast(h=forecast.horizon)
        
        
        auto_exp_accuracy <- as.data.frame(accuracy( patient_train_auto_exp_forecast ,patient.test))
        auto_arima_accuracy <- as.data.frame(accuracy(patient_train_auto_arima_forecast ,patient.test))
        simple_exp_accuracy <- as.data.frame(accuracy(patient_train_simple_exp_forecast ,patient.test))
        double_exp_accuracy <- as.data.frame(accuracy(patient_train_double_exp_forecast ,patient.test))
        triple_exp_accuracy <- as.data.frame(accuracy(patient_train_triple_exp_forecast ,patient.test))
        tbat_accuracy <- as.data.frame(accuracy(patient_train_tbat_forecast ,patient.test))
        #manual accuracy
        manual_accuracy <- as.data.frame(accuracy(patient_train_manual_forecast ,patient.test))
        
        numeric_update <- function(df){
            rownames(df) <- c()
            is.num <- sapply(df, is.numeric)
            df[is.num] <- lapply(df[is.num], round, 2)           
            return (df)
        }
        if (length(manual_accuracy) < 1 ){
            auto_exp_accuracy <- numeric_update(auto_exp_accuracy)
            auto_arima_accuracy <- numeric_update(auto_arima_accuracy)
            simple_exp_accuracy <- numeric_update(simple_exp_accuracy)
            double_exp_accuracy <- numeric_update(double_exp_accuracy)
            triple_exp_accuracy <- numeric_update(triple_exp_accuracy)
            tbat_accuracy <- numeric_update(tbat_accuracy)
            
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
        } else {
            auto_exp_accuracy <- numeric_update(auto_exp_accuracy)
            auto_arima_accuracy <- numeric_update(auto_arima_accuracy)
            simple_exp_accuracy <- numeric_update(simple_exp_accuracy)
            double_exp_accuracy <- numeric_update(double_exp_accuracy)
            triple_exp_accuracy <- numeric_update(triple_exp_accuracy)
            tbat_accuracy <- numeric_update(tbat_accuracy)
            # manual accuracy
            manual_accuracy <- numeric_update(manual_accuracy)
            
            models<- c("auto-exponential","auto-exponential",
                       "auto-arima","auto-arima",
                       "simple-exponential","simple-exponential",
                       "double-exponential","double-exponential",
                       "triple-exponential","triple-exponential",
                       "tbat","tbat",
                       "manual-arima","manual-arima")
            
            data<- c("Training set", 'Test set',
                     "Training set", 'Test set',
                     "Training set", 'Test set',
                     "Training set", 'Test set',
                     "Training set", 'Test set',
                     "Training set", 'Test set',
                     "Training set", 'Test set')
            
            outputInfo <- rbind(auto_exp_accuracy,auto_arima_accuracy,
                                simple_exp_accuracy,double_exp_accuracy,
                                triple_exp_accuracy,tbat_accuracy,manual_accuracy)           
        }
        

        
        outputInfo <- cbind(models, data, outputInfo)
        

        # model output
        auto_arima <- "auto-arima"        %in% input$modelInput
        auto_exp   <- 'auto-exponential'  %in% input$modelInput
        simple_exp <- "simple-exponential" %in% input$modelInput
        double_exp <- "double-exponential" %in% input$modelInput
        triple_exp <- "triple-exponential" %in% input$modelInput
        tbat <- "tbat"  %in% input$modelInput
        manual_arima <- "manual-arima"  %in% input$modelInput
        
        model_selection <- unlist(strsplit(input$modelInput, split=" "))
        model_count <- length(model_selection)
        
        if (is.null(input$modelInput)){
        
        } else {
            outputInfo <- outputInfo %>% 
                filter(models %in% model_selection)   
        }
        
        # forecast accuracy output
        DT::datatable(outputInfo, options = list(scrollX = TRUE))
        
    })
    

    
}

shinyApp(ui, server)

#old code
#if (is.null(input$modelInput)){

# } else if (model_count == 1){
#     if (auto_arima){
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("auto-arima"))
#     } else if (auto_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("auto-exponential"))
#     } else if (simple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("simple-exponential"))
#     } else if (double_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("double-exponential"))
#     } else if (triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("triple-exponential"))
#     } else if (tbat ) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("tbat"))
#     } else if(manual_arima){
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("manual-arima"))
#     }
# } else if (model_count == 2){
#     if(auto_arima &  auto_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("auto-arima","auto-exponential")) 
#     } else if(auto_arima &  simple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("auto-arima","simple-exponential"))
#     } else if(auto_arima &  double_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("auto-arima","double-exponential"))
#     } else if(auto_arima &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("auto-arima","triple-exponential"))
#     } else if(auto_arima &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("auto-arima","tbat"))
#     } else if(auto_exp &  simple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("auto-exponential","simple-exponential"))
#     } else if(auto_exp &  double_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("auto-exponential","double-exponential"))                
#     }else if(auto_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("auto-exponential","triple-exponential"))
#     } else if(auto_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("tbat","auto-exponential"))
#     }else if(simple_exp &  double_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("simple-exponential","double-exponential"))
#     }else if(simple_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("simple-exponential","triple-exponential"))                
#     }else if(simple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("tbat","simple-exponential"))
#     }else if(double_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("double-exponential","triple-exponential"))
#     }else if(double_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("tbat","double-exponential"))
#     }else if(triple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c("tbat","triple-exponential"))
#     }else if(auto_exp & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'manual-arima') )
#     }else if(auto_arima & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-arima', 'manual-arima') )
#     }else if(simple_exp & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('simple-exponential', 'manual-arima') )
#     }else if(double_exp & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('double-exponential', 'manual-arima') )
#     }else if(triple_exp & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('triple-exponential', 'manual-arima') )
#     }else if(tbat & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('tbat', 'manual-arima') )
#     }
#     
#     
# } else if (model_count == 3) {
#     if(auto_arima &  auto_exp &  simple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat","double-exponential","triple-exponential"))
#     }else if(auto_arima &  auto_exp &  double_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat","simple-exponential","triple-exponential"))
#     }else if(auto_arima &  auto_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat","simple-exponential","double-exponential"))
#     }else if(auto_arima &  auto_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("triple-exponential","double-exponential","simple-exponential"))             
#     }else if(auto_arima &  simple_exp &  double_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat","auto-exponential","triple-exponential"))
#     }else if(auto_arima &  simple_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat","double-exponential","auto-exponential"))
#     }else if(auto_arima &  simple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("double_exponential","auto-exponential","triple-exponential"))
#     }else if(auto_arima &  double_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat","auto-exponential","simple-exponential"))
#     }else if(auto_arima &  double_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("simple-exponential","auto-exponential","triple-exponential"))
#     }else if(auto_arima &  triple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("double-exponential","auto-exponential","simple-exponential"))
#     }else if(auto_exp &  simple_exp &  double_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","tbat","triple-exponential"))                
#     }else if(auto_exp &  simple_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","tbat","double-exponential"))
#     }else if(auto_exp &  simple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","double-exponential","triple-exponential"))               
#     }else if(auto_exp &  double_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","tbat","simple-exponential"))
#     }else if(auto_exp &  double_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","triple-exponential","simple-exponential"))
#     }else if(auto_exp &  triple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","double-exponential","simple-exponential"))
#     }else if(simple_exp &  double_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","auto-exponential","tbat"))
#     }else if(simple_exp &  double_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","auto-exponential","triple-exponential"))
#     }else if(simple_exp &  triple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","auto-exponential","double-exponential"))
#     }else if(double_exp &  triple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","auto-exponential","simple-exponential"))
#     }
# } else if (model_count == 4){
#     if(auto_arima &  auto_exp &  simple_exp &  double_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat","triple-exponential"))
#     }else if(auto_arima &  auto_exp &  simple_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat","double-exponential"))
#     }else if(auto_arima &  auto_exp &  simple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("double-exponential","triple-exponential"))
#     }else if(auto_arima &  auto_exp &  double_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat","simple-exponential")) 
#     }else if(auto_arima &  auto_exp &  double_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("simple-exponential","triple-exponential"))
#     }else if(auto_arima &  auto_exp &  triple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("simple-exponential","double-exponential"))
#     }else if(auto_arima &  simple_exp &  double_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat","auto-exponential"))
#     }else if(auto_arima &  simple_exp &  double_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("triple-exponential","auto-exponential")) 
#     }else if(auto_arima &  simple_exp &  triple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("double-exponential","auto-exponential"))               
#     }else if(auto_arima &  double_exp &  triple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("simple-exponential","auto-exponential"))
#     }else if(auto_exp &  simple_exp &  double_exp &  triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat","auto-exponential"))
#     }else if(auto_exp &  simple_exp &  double_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","triple-exponential"))
#     }else if(auto_exp &  simple_exp &  triple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","double-exponential"))
#     }else if(auto_exp &  double_exp &  triple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","simple-exponential"))
#     }else if(simple_exp &  double_exp &  triple_exp &  tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima","auto-exponential"))
#     }
# } else if (model_count == 5){
#     if (auto_arima & auto_exp & simple_exp & double_exp & triple_exp) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("tbat"))
#     } else if (auto_arima & simple_exp & double_exp & triple_exp & tbat) {
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-exponential"))
#     } else if (auto_arima & auto_exp & simple_exp & double_exp & tbat){
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("triple-exponential"))             
#     } else if (auto_arima & auto_exp & simple_exp &  triple_exp & tbat){
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("double-exponential"))
#     } else if (auto_arima & auto_exp & double_exp & triple_exp & tbat){
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("simple-exponential"))
#     } else if (auto_exp & simple_exp & double_exp & triple_exp & tbat){
#         outputInfo <- outputInfo %>% 
#             filter(!models %in% c("auto-arima"))
#     } else if() {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c ) 
#     } else if() {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c ) 
#     }else if(auto_exp & auto_arima & simple_exp & tbat & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'auto-arima', 'simple-exponential', 'tbat', 'manual-arima') ) 
#     }else if(auto_exp & auto_arima & double_exp & triple_exp & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'auto-arima', 'double-exponential', 'triple-exponential', 'manual-arima') ) 
#     }else if(auto_exp & auto_arima & double_exp & tbat & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'auto-arima', 'double-exponential', 'tbat', 'manual-arima') ) 
#     }else if(auto_exp & auto_arima & triple_exp & tbat & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'auto-arima', 'triple-exponential', 'tbat', 'manual-arima') ) 
#     }else if(auto_exp & simple_exp & double_exp & triple_exp & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'simple-exponential', 'double-exponential', 'triple-exponential', 'manual-arima') ) 
#     }else if(auto_exp & simple_exp & double_exp & tbat & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'simple-exponential', 'double-exponential', 'tbat', 'manual-arima') ) 
#     }else if(auto_exp & simple_exp & triple_exp & tbat & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'simple-exponential', 'triple-exponential', 'tbat', 'manual-arima') ) 
#     }else if(auto_exp & double_exp & triple_exp & tbat & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'double-exponential', 'triple-exponential', 'tbat', 'manual-arima') ) 
#     }else if(auto_arima & simple_exp & double_exp & triple_exp & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-arima', 'simple-exponential', 'double-exponential', 'triple-exponential', 'manual-arima') ) 
#     }else if(auto_arima & simple_exp & double_exp & tbat & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-arima', 'simple-exponential', 'double-exponential', 'tbat', 'manual-arima') ) 
#     }else if(auto_arima & simple_exp & triple_exp & tbat & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-arima', 'simple-exponential', 'triple-exponential', 'tbat', 'manual-arima') ) 
#     }else if(auto_arima & double_exp & triple_exp & tbat & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-arima', 'double-exponential', 'triple-exponential', 'tbat', 'manual-arima') ) 
#     }else if(simple_exp & double_exp & triple_exp & tbat & manual_arima) {
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('simple-exponential', 'double-exponential', 
#                                  'triple-exponential', 'tbat', 'manual-arima') ) 
#     }
# } else if (model_count == 6){
#     if(auto_exp & auto_arima & simple_exp & double_exp & triple_exp & tbat){
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'auto-arima', 'simple-exponential', 
#                                  'double-exponential', 'triple-exponential', 'tbat') ) 
#     } else if (auto_exp & auto_arima & simple_exp & double_exp & triple_exp & manual_arima){
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'auto-arima', 'simple-exponential', 'double-exponential', 
#                                  'triple-exponential', 'manual-arima') ) 
#     }else if (auto_exp & auto_arima & simple_exp & double_exp & tbat & manual_arima){
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'auto-arima', 'simple-exponential', 
#                                  'double-exponential', 'tbat', 'manual-arima') ) 
#     }else if (auto_exp & auto_arima & simple_exp & triple_exp & tbat & manual_arima){
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'auto-arima', 'simple-exponential', 
#                                  'triple-exponential', 'tbat', 'manual-arima') ) 
#     }else if (auto_exp & auto_arima & double_exp & triple_exp & tbat & manual_arima){
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'auto-arima', 'double-exponential',
#                                  'triple-exponential', 'tbat', 'manual-arima') ) 
#     }else if (auto_exp & simple_exp & double_exp & triple_exp & tbat & manual_arima){
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-exponential', 'simple-exponential', 'double-exponential', 
#                                  'triple-exponential', 'tbat', 'manual-arima') ) 
#     }else if (auto_arima & simple_exp & double_exp & triple_exp & tbat & manual_arima){
#         outputInfo <- outputInfo %>% 
#             filter(models %in% c('auto-arima', 'simple-exponential', 'double-exponential', 
#                                  'triple-exponential', 'tbat', 'manual-arima') ) 
#     }
# }
# 
