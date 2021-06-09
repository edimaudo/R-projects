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

#data 
patient.xts <- xts(x = df$Patients, order.by = df$Arrival_date) 
#aggregations
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
                            fluidRow(
                                h3("Decomposition",style="text-align: center;"),
                                plotOutput("decompositionPlot"),
                                h3("ACF/PACF",style="text-align: center;"),
                                h4("ACF Plot",style="text-align: center;"),
                                plotOutput("acfPlot"),
                                br(),
                                h4("PACF Plot",style="text-align: center;"),
                                plotOutput("pacfPlot"),
                                h3("Forecast",style="text-align: center;"),
                                plotOutput("forecastPlot")
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
        patient.train.components <- decompose(patient.train)
        plot(patient.train.components)
        
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
            acf(patient.train)
        } else {
            acf(log(patient.train))
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
            pacf(patient.train)
        } else {
            pacf(log(patient.train))
        }
        
    })
    
    output$forecastPlot <- renderPlot({
        #aggregations
        patient.daily <- apply.daily(patient.xts,mean)
        patient.weekly <- apply.weekly(patient.xts, mean) 
        patient.monthly <- apply.monthly(patient.xts, mean) 
        
        patient.end <- floor(0.8*length(patient.daily)) #select the first 80% of the data
        patient.train <- patient.daily[1:patient.end,] 
        patient.test <- patient.daily[(patient.end+1):length(patient.daily),]
        
        patient.start <- c(year (start(patient.train)), month(start(patient.train)),day(start(patient.train)))
        patient.end <- c(year(end(patient.train)), month(end(patient.train)), day(end(patient.train)))
        patient.train <- ts(as.numeric(patient.train), start = patient.start, 
                            end = patient.end, frequency = 7)
        
        patient.start <- c(year (start(patient.test)), month(start(patient.test)),day(start(patient.test)))
        patient.end <- c(year(end(patient.test)), month(end(patient.test)), day(end(patient.test)))
        patient.test <- ts(as.numeric(patient.test), start = patient.start, 
                           end = patient.end, frequency = 7)
        
        
        #set forecast horizon
        
        #Exponential Smoothing Forecast account for the trend and seasonal components
        patient.train.esforecast <- HoltWinters(patient.train,
                                                beta=TRUE, 
                                                gamma=TRUE) %>% 
            forecast(h=forecast.horizon)
        
        
        fit_ets <- ets(patient.train)
        # Automated forecasting using an ARIMA model
        fit_arima <- auto.arima(patient.train)
        
        patient.train%>% 
            HoltWinters(beta = TRUE, gamma = TRUE) %>% 
            forecast(h=forecast.horizon) %>% 
            plot()
        lines(patient.test, col = "red")
        
        #Auto forecast
        patient.train %>%
            forecast(h=forecast.horizon) %>% 
            plot()
        lines(patient.test, col = "red")
        
        #Model accuracy
        accuracy(patient.train.esforecast,patient.test)
        
    })
    
    
}

shinyApp(ui, server)


