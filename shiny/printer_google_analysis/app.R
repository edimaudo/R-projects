# User content sentiment analysis
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
# load data
#=============
df <- read.csv("reviews_v2.csv")

#=============
# data cleaning
#=============
df[df==0] <- NA #assigne 0 to NA
df <- na.omit(df) #remove na

#=============
# Add printer column
#=============
df$printer <- ifelse(df$appId == "com.hp.printercontrol", 'HP',
                     ifelse(df$appId == "jp.co.canon.bsd.ad.pixmaprint", 'Canon',
                            ifelse(df$appId == "epson.print", 'Epson', 'Epson Smart')))

#=============
# Dropdown information
#=============                        
printer_info <- c('Canon','Epson','Epson Smart','HP')

# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "Printer Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("th")),
            menuItem("Analysis", tabName = "city", icon = icon("th")),
            menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview",includeMarkdown("readme.md"),hr()),
            tabItem(tabName = "Analysis",
                    sidebarLayout(
                        sidebarPanel(
                            checkboxGroupInput("printerInput", "Printer",choices = printer_info, 
                                               selected = printer_info),
                            # selectInput("horizonInput", "Horizon", 
                            #             choices = horizon_info, selected = 14),
                            # selectInput("frequencyInput", "Frequency", 
                            #             choices = frequency_info, selected = 7),
                            # radioButtons("differenceInput","Difference",
                            #              choices = difference_info, selected = "No"),
                            # radioButtons("logInput","Log",
                            #              choices = log_info, selected = "No"),
                            # selectInput("modelInput", "Model", 
                            #             choices = model_info, selected = 'auto exponential'),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Forecast Analysis",style="text-align: center;")#, 
                            # tabsetPanel(type = "tabs",
                            #             tabPanel(h4("Decomposition",style="text-align: center;"), 
                            #                      plotOutput("decompositionPlot")),
                            #             tabPanel(h4("ACF Plot",style="text-align: center;"), 
                            #                      plotOutput("acfPlot")),
                            #             tabPanel(h4("PACF Plot",style="text-align: center;"), 
                            #                      plotOutput("pacfPlot")),
                            #             tabPanel(h4("Forecast Output",style="text-align: center;"), 
                            #                      plotOutput("forecastPlot")),
                            #             tabPanel(h4("Forecast Accuracy",style="text-align: center;"), 
                            #                      DT::dataTableOutput("accuracyOutput"))
                            )
                        )
                    )
            ) 
        )
    )
#)

# Define server logic 
server <- function(input, output,session) {
    
}

shinyApp(ui, server)