################
# Sales Insights
################

rm(list = ls()) # clear environment

################
# Libraries
################
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl','scales',
              'dplyr','mlbench','caTools','forecast','TTR','xts','lubridate','shiny',
              'shinydashboard')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

################
# Load Data
################
df <- read_excel("Sales.xlsx")

################
# Application UI
################

item_code_dropdown <- sort(as.vector(unique(df$`Item Code`))) # Item code 
item_code_dropdown <- c("All",item_code_dropdown)
country_dropdown <- sort(as.vector(unique(df$Country))) # Country
country_dropdown<- c("All",country_dropdown)

horizon_info  <- c(1:100) #forecast range
aggregate_info <- c('daily','weekly','monthly')
frequency_info <- c(7, 12, 52, 365)
difference_info <- c("Yes","No")
log_info <- c("Yes","No")
model_info <- c('auto arima','auto exponential','simple exponential',
                'double exponential','triple exponential')

ui <- dashboardPage(
    dashboardHeader(title = "Sales Insights & Prediction"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data", tabName = "data", icon = icon("th")),
            menuItem("Analysis", tabName = "analysis", icon = icon("th")),
            menuItem("Forecasting", tabName = "Forecast", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            #--------------
            # data UI
            #--------------
            tabItem(tabName = "data",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("itemCodeInput", "Item Code", choices = item_code_dropdown),
                            selectInput("countryInput", "Country", choices = country_dropdown),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Sales Overview",style="text-align: center;"), 
                            # Sales Trend
                            plotOutput("salesTrendPlot"),
                            br(),
                            br(),
                            # Sales data
                            DT::dataTableOutput("salesOutput")
                        )
                    )
            ),
            #--------------
            # analysis UI
            #--------------
            tabItem(tabName = "analysis",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("itemCodeInput", "Item Code", choices = item_code_dropdown),
                            selectInput("countryInput", "Country", choices = country_dropdown),
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
                            br(),
                            DT::dataTableOutput("testOutput"),
                            br(),
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
            
            )
        )
        
    )
)
    


#----------------------
# Sales Information
#----------------------
sales_info <- function(item,country){
    
    if (item == 'All' && country == "All"){
        sales_df <- df %>%
            group_by(Date) %>%
            summarise(Quantity_total = sum(`Item Code`)) %>%
            select(Date, Quantity_total )
    } else if (item != 'All' && country == "All"){
        sales_df <- df %>%
            filter(`Item Code`  == item) %>%
            group_by(Date) %>%
            summarise(Quantity_total = sum(`Item Code`)) %>%
            select(Date, Quantity_total )
    } else if (item == 'All' && country != "All"){
        sales_df <- df %>%
            filter(Country == country) %>%
            group_by(Date) %>%
            summarise(Quantity_total = sum(`Item Code`)) %>%
            select(Date, Quantity_total )
    } else {
        sales_df <- df %>%
            filter(`Item Code`  == item,Country == country) %>%
            group_by(Date) %>%
            summarise(Quantity_total = sum(`Item Code`)) %>%
            select(Date, Quantity_total )
    }
}

#----------------------
# Analysis Information
#----------------------
analysis_info <- function(item, country,aggregateType, frequency){
    if (item == 'All' && country == "All"){
        analysis_df <- df %>%
            group_by(Date) %>%
            summarise(Quantity_total = sum(`Item Code`)) %>%
            select(Date, Quantity_total )
    } else if (item != 'All' && country == "All"){
        analysis_df <- df %>%
            filter(`Item Code`  == item) %>%
            group_by(Date) %>%
            summarise(Quantity_total = sum(`Item Code`)) %>%
            select(Date, Quantity_total )
    } else if (item == 'All' && country != "All"){
        analysis_df <- df %>%
            filter(Country == country) %>%
            group_by(Date) %>%
            summarise(Quantity_total = sum(`Item Code`)) %>%
            select(Date, Quantity_total )
    } else {
        analysis_df <- df %>%
            filter(`Item Code`  == item,Country == country) %>%
            group_by(Date) %>%
            summarise(Quantity_total = sum(`Item Code`)) %>%
            select(Date, Quantity_total )
    }

    
    analysis.xts <- xts(analysis_df$Quantity_total, order.by = analysis_df$Date) 
    analysis.daily <- apply.daily(analysis.xts,mean)
    analysis.weekly <- apply.weekly(analysis.xts, mean) 
    analysis.monthly <- apply.monthly(analysis.xts, mean) 
    
    if (aggregateType== 'daily'){
        analysis.end <- floor(1*length(analysis.daily)) 
        analysis.data <- analysis.daily[1:analysis.end,] 
        analysis.start <- c(year (start(analysis.data)), month(start(analysis.data)),
                            day(start(analysis.data)))
        analysis.end <- c(year(end(analysis.data)), month(end(analysis.data)), 
                          day(end(analysis.data)))
        analysis.data <- ts(as.numeric(analysis.data), start = analysis.start, 
                            end = analysis.end, frequency = as.numeric(frequency))             
    } else if(aggregateType== 'weekly'){
        analysis.end <- floor(1*length(analysis.weekly)) 
        analysis.data <- analysis.weekly[1:analysis.end,] 
        analysis.start <- c(year (start(analysis.data)), month(start(analysis.data)),
                            week(start(analysis.data)))
        analysis.end <- c(year(end(analysis.data)), month(end(analysis.data)), 
                          week(end(analysis.data)))
        analysis.data <- ts(as.numeric(analysis.data), start = analysis.start, 
                            end = analysis.end, frequency = as.numeric(frequency))         
    } else {
        analysis.end <- floor(1*length(analysis.monthly)) 
        analysis.data <- analysis.monthly[1:analysis.end,] 
        analysis.start <- c(year (start(analysis.data)), month(start(analysis.data)))
        analysis.end <- c(year(end(analysis.data)), month(end(analysis.data)))
        analysis.data <- ts(as.numeric(analysis.data), start = analysis.start, 
                            end = analysis.end, frequency = as.numeric(frequency))               
    } 
   
}


################
# Server
################
server <- function(input, output,session) {
    
    sales_output <- reactive({sales_info(input$itemCodeInput,input$countryInput)})
    analysis_output <- reactive({analysis_info(input$itemCodeInput,input$countryInput,
                                               input$aggregateInput, input$frequencyInput)})
    
    #--------------------
    # Sales Trend Plot
    #--------------------
    output$salesTrendPlot <- renderPlot({
        sales_output_df <- as.data.frame(sales_output())
        sales_Qty <- xts::xts(sales_output_df$Quantity_total, order.by = sales_output_df$Date) 
        autoplot(sales_Qty)
    })
    
    #--------------------
    # Sales Output table
    #--------------------
    output$salesOutput <- DT::renderDataTable({
        sales_output()
    })
    
    #====================
    # Analysis
    #====================
    
   
    
    # decomposition output
    output$decompositionPlot <- renderPlot({
        
        analysis.data <- analysis_output()
        
        if (input$differenceInput == "Yes"){
            analysis.data <- diff(analysis_output(), 
                                  differences = as.numeric(input$differenceNumericInput)) 
        }
        #Decompose the Time Series
        analysis.data %>%
            decompose() %>%
            autoplot()
        
    })
    
    # multi season output
    output$multidecompositionPlot <- renderPlot({
        analysis.data <- analysis_output()
        
        if (input$differenceInput == "Yes"){
            analysis.data <- diff(analysis.data, differences = as.numeric(input$differenceNumericInput))
        }
        #Decompose the Time Series
        analysis.data %>%
            mstl() %>%
            autoplot()
        
    })
    
    # ACF output
    output$acfPlot <- renderPlot({
        analysis.data <- analysis_output()
        if (input$logInput == "No"){
            ggAcf(analysis.data)
        } else {
            ggAcf(log(analysis.data))
        }
    })
    
    # PACF output
    output$pacfPlot <- renderPlot({
        analysis.data <- analysis_output()
        if (input$logInput == "No"){
            ggPacf(analysis.data) #pacf(patient.train)
        } else {
            ggPacf(log(analysis.data))
        }
    })
    
    
}


shinyApp(ui, server)