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

#===============
# Data Munging
#===============

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
            tabItem(tabName = "data",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("itemCodeInput", "Item Code", choices = item_code_dropdown),
                            selectInput("countryInput", "Country", choices = country_dropdown),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Analysis",style="text-align: center;"), 
                            # Sales Trend
                            plotOutput("salesTrendPlot"),
                            # Sales Info
                            tabsetPanel(type = "tabs",
                                        tabPanel(
                                            h4("Top 5 Quantity by Country",
                                               style="text-align: center;"),
                                            plotOutput("topCountryPlot")),
                                        tabPanel(
                                            h4("Bottom 5 Quantity by Country",
                                               style="text-align: center;"),
                                            plotOutput("bottomCountryPlot")),
                                        tabPanel(
                                            h4("Top 5 Quantity by Item Code",
                                               style="text-align: center;"), 
                                            plotOutput("itemTopPlot")),
                                        tabPanel(
                                            h4("Bottom 5 Quantity by Item Code",
                                               style="text-align: center;"), 
                                            plotOutput("itemBottomPlot"))
                            ),
                            
                            # Sales data
                            DT::dataTableOutput("salesOutput")
                        )
                    )
            )
        )
        )
    )
    


#----------------------
# Get sales information
#----------------------
sales_info <- function(item,country){
    
    if (item== 'All' && country == "All"){
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

################
# Server
################
server <- function(input, output,session) {
    
    sales_output <- reactive({ sales_info(input$itemCodeInput,input$countryInput) })
    
    #--------------------
    # Sales Trend Graph
    #--------------------
    output$salesTrendPlot <- renderPlot({
        
    })
    
    #--------------------
    # Sales Output table
    #--------------------
    output$topCountryPlot <- renderPlot({
        
    })
    
    #--------------------
    # Sales Output table
    #--------------------
    output$bottomCountryPlot <- renderPlot({
        
    })
    
    #--------------------
    # Sales Output table
    #--------------------
    output$itemTopPlot <- renderPlot({
        
    })
    
    #--------------------
    # Sales Output table
    #--------------------
    output$itemBottomPlot <- renderPlot({
        
    })
    #--------------------
    # Sales Output table
    #--------------------
    output$salesOutput <- DT::renderDataTable({
        sales_output()
    })
    
}


shinyApp(ui, server)